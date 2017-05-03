;;; CRAWLIK patches to XML to handle malformed documents
;;; see LICENSE file for permissions

(in-package #:cxml)

(defparameter *html5-ztags* '(:area :base :br :col :embed :hr :img :input
                              :keygen :link :meta :param :source :track :wbr))

(defstruct (context (:conc-name nil))
  handler
  (dtd nil)
  model-stack
  base-stack
  tag-stack
  (referenced-notations '())
  (id-table (make-rod-hashtable))
  (name-hashtable (make-rod-hashtable :size 2000))
  (standalone-p nil)
  (entity-resolver nil)
  (disallow-internal-subset nil)
  main-zstream
  skip-end-tags)

(defun find-namespace-binding (prefix)
  (cdr (or (assoc (or prefix #"") *namespace-bindings* :test #'rod=)
           (restart-case (wf-error nil "Undeclared namespace prefix: ~A"
                                         (rod-string prefix))
             (ignore-namespace ()
               (push (cons prefix "") *namespace-bindings*))))))

(defun read-tag-2 (zinput input kind)
  (let ((name (read-name-token input))
        (atts nil))
    (setf atts (restart-case (read-attribute-list zinput input nil)
                 (skip ()
                   (loop :until (eq #\> (peek-rune input)) :do
                     (consume-rune input))
                   nil)))
    ;; check for double attributes
    (do ((q atts (cdr q)))
        ((null q))
      (when (find (caar q) (cdr q) :key #'car)
        (restart-case
            (wf-error zinput "Attribute ~S has two definitions in element ~S."
                      (rod-string (caar q))
                      (rod-string name))
          (ignore-double-attr () nil))))
    (cond ((eq (peek-rune input) #/>)
           (consume-rune input)
           (values kind (cons name atts)))
          ((eq (peek-rune input) #//)
           (consume-rune input)
           (check-rune input #/> (read-rune input))
           (values :ztag (cons name atts)))
          (t
           (wf-error zinput "syntax error in read-tag-2.")) )))

(defun p/document
    (input handler
     &key validate dtd root entity-resolver disallow-internal-subset
       (recode t))
  ;; check types of user-supplied arguments for better error messages:
  (check-type validate boolean)
  (check-type recode boolean)
  (check-type dtd (or null extid))
  (check-type root (or null rod))
  (check-type entity-resolver (or null function symbol))
  (check-type disallow-internal-subset boolean)
  #+rune-is-integer
  (when recode
    (setf handler (make-recoder handler #'rod-to-utf8-string)))
  (let* ((xstream (car (zstream-input-stack input)))
         (name (xstream-name xstream))
         (base (when name (stream-name-uri name)))
         (*ctx*
           (make-context :handler handler
                         :main-zstream input
                         :entity-resolver entity-resolver
                         :base-stack (list (or base ""))
                         :disallow-internal-subset disallow-internal-subset))
         (*validate* validate)
         (*namespace-bindings* *initial-namespace-bindings*))
    (sax:register-sax-parser handler (make-instance 'cxml-parser :ctx *ctx*))
    (sax:start-document handler)
    ;; document ::= XMLDecl? Misc* (doctypedecl Misc*)? element Misc*
    ;; Misc ::= Comment | PI |  S
    ;; xmldecl::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    ;; sddecl::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
    (let ((*data-behaviour* :DTD))
      ;; optional XMLDecl?
      (p/xmldecl input)
      ;; Misc*
      (p/misc*-2 input)
      ;; (doctypedecl Misc*)?
      (cond
        ((eq (peek-token input) :<!DOCTYPE)
         (p/doctype-decl input dtd)
         (p/misc*-2 input))
        (dtd
         (synthesize-doctype dtd input))
        ((and validate (not dtd))
         (validity-error "invalid document: no doctype")))
      (ensure-dtd)
      ;; Override expected root element if asked to
      (when root
        (setf (model-stack *ctx*) (list (make-root-model root))))
      ;; element
      (let ((*data-behaviour* :DOC))
        (fix-seen-< input)
        (p/element input))
      ;; optional Misc*
      (p/misc*-2 input)
      (p/eof input)
      (sax:end-document handler))))

(defvar *cur* nil)

(defun p/element (input)
  (multiple-value-bind (cat n-b new-b uri lname qname attrs) (p/sztag input)
    (push lname (tag-stack *ctx*))
    (sax:start-element (handler *ctx*) uri lname qname attrs)
    (when (and (eq cat :stag)
               #+nil (not (member (rutil:mkeyw lname) *html5-ztags*)))
      (loop (let ((*namespace-bindings* n-b))
              (p/content input))
            (restart-case (progn
                            (if (rutil:and-it (skip-end-tags *ctx*)
                                              (plusp rutil:it))
                                (decf (skip-end-tags *ctx*))
                                (p/etag input qname))
                            (return))
              ;; (skip-end-tag ()
              ;;   (print (cons :skip lname))
              ;;   nil)
              (close-end-tag ()
                (rutil:when-it (position *cur* (tag-stack *ctx*)
                                         :test 'equalp)
                  (print (tag-stack *ctx*))
                  (when (< rutil:it 2)
                    (print (tag-stack *ctx*))
                    (setf (skip-end-tags *ctx*) (1+ rutil:it))
                    (loop :repeat (1- rutil:it) :do
                      (pop (base-stack *ctx*)))
                    (return)))))))
    (princ ".")
    (sax:end-element (handler *ctx*) uri lname qname)
    (undeclare-namespaces new-b)
    (pop (base-stack *ctx*))
    (pop (tag-stack *ctx*))
    (validate-end-element *ctx* qname)))

(defun p/etag (input qname)
  (multiple-value-bind (cat2 sem2) (read-token input)
    (unless (and (eq cat2 :etag)
                 (eq (car sem2) qname))
      (:= *cur* (car sem2))
      (print (list :q *cur* qname))
      (wf-error input "Bad nesting. ~S / ~S"
                (mu qname)
                (mu (cons cat2 sem2))))
    (rutil:void *cur*)
    (when (cdr sem2)
      (wf-error input "no attributes allowed in end tag"))))

(defun p/eof (input)
  (unless (eq (peek-token input) :eof)
    (restart-case (wf-error input "Garbage at end of document.")
      (finish-processing () nil)))
  (when *validate*
    (maphash (lambda (k v)
               (unless v
                 (validity-error "(11) IDREF: ~S not defined" (rod-string k))))
             (id-table *ctx*))
    (dolist (name (referenced-notations *ctx*))
      (unless (find-notation name (dtd *ctx*))
        (validity-error "(23) Notation Declared: ~S" (rod-string name))))))

(defun read-token-after-|<| (zinput input)
  (let ((d (read-rune input)))
    (cond ((eq d :eof)
           (eox input "EOF after '<'"))
          ((rune= #/! d)
           (read-token-after-|<!| input))
          ((rune= #/? d)
           (multiple-value-bind (target content) (read-pi input)
             (cond ((rod= target '#.(string-rod "xml"))
                    (values :xml-decl (cons target content)))
                   ((rod-equal target '#.(string-rod "XML"))
                    (wf-error zinput
                              "You lost -- no XML processing instructions."))
                   ((and sax:*namespace-processing* (position #/: target))
                    (wf-error zinput
                              "Processing instruction target ~S is not a ~
                               valid NcName."
                              (mu target)))
                   (t
                    (values :PI (cons target content))))))
          ((eq *data-behaviour* :DTD)
           (unread-rune d input)
           (unless (or (rune= #// d) (name-start-rune-p d))
             (wf-error zinput "Expected '!' or '?' after '<' in DTD."))
           (values :seen-< nil))
          ((rune= #// d)
           (let ((c (peek-rune input)))
             (restart-case
                 (cond ((name-start-rune-p c)
                        (read-tag-2 zinput input :etag))
                       (t
                        (wf-error zinput
                                  "Expecting name start rune after \"</\".")))
               (skip-end-tag ()
                 (read-token-3 zinput)))))
          ((name-start-rune-p d)
           (unread-rune d input)
           (read-tag-2 zinput input :stag))
          (t
           (restart-case (wf-error zinput "Expected '!' or '?' after '<' in DTD.")
             (treat-<-as-lt ()
               (unread-rune d input)
               (values :cdata "<")))))))

(defun read-token-3 (zinput)
  (let ((input (car (zstream-input-stack zinput))))
    ;; PI Comment
    (let ((c (read-rune input)))
      (cond
        ;; first the common tokens
        ((rune= #/< c)
         (read-token-after-|<| zinput input))
        ;; now dispatch
        (t
         (ecase *data-behaviour*
           (:DTD
            (cond ((rune= #/\[ c) :\[)
                  ((rune= #/\] c) :\])
                  ((rune= #/\( c) :\()
                  ((rune= #/\) c) :\))
                  ((rune= #/\| c) :\|)
                  ((rune= #/\> c) :\>)
                  ((rune= #/\" c) :\")
                  ((rune= #/\' c) :\')
                  ((rune= #/\, c) :\,)
                  ((rune= #/\? c) :\?)
                  ((rune= #/\* c) :\*)
                  ((rune= #/\+ c) :\+)
                  ((name-rune-p c)
                   (unread-rune c input)
                   (values :nmtoken (read-name-token input)))
                  ((rune= #/# c)
                   (let ((q (read-name-token input)))
                     (cond ((rod= q '#.(string-rod "REQUIRED")) :|#REQUIRED|)
                           ((rod= q '#.(string-rod "IMPLIED")) :|#IMPLIED|)
                           ((rod= q '#.(string-rod "FIXED"))   :|#FIXED|)
                           ((rod= q '#.(string-rod "PCDATA"))  :|#PCDATA|)
                           (t
                            (wf-error zinput "Unknown token: ~S." q)))))
                  ((or (rune= c #/U+0020)
                       (rune= c #/U+0009)
                       (rune= c #/U+000D)
                       (rune= c #/U+000A))
                   (values :S nil))
                  ((rune= #/% c)
                   (cond ((name-start-rune-p (peek-rune input))
                          ;; an entity reference
                          (read-pe-reference zinput))
                         (t
                          (values :%))))
                  (t
                   (wf-error zinput "Unexpected character ~S." c))))
           (:DOC
            (cond
              ((rune= c #/&)
               (restart-case
                   (multiple-value-bind (kind data) (read-entity-like input)
                     (cond ((eq kind :ENTITY-REFERENCE)
                            (values :ENTITY-REF data))
                           ((eq kind :CHARACTER-REFERENCE)
                            (values :CDATA
                                    (with-rune-collector (collect)
                                      (%put-unicode-char data collect))))))
                 (treat-single-&-as-is ()
                   (values :CDATA (make-array 1 :element-type 'rune
                                              :initial-contents '(#\&))))))
              (t
               (unread-rune c input)
               (values :CDATA (read-cdata input)))))))))))

