;;; CRAWLIK HTML matching lang
;;; see LICENSE file for permissions

(in-package #:crawlik)
(named-readtables:in-readtable rutilsx-readtable)


(defvar *tree*)
(defvar *vars*)

(define-condition html-matched () ())
(define-condition finish-matching () ())

(define-symbol-macro !!!
    (progn (signal 'html-matched) t))

(defun match-html (source expr)
  (:= expr (maptree ^(if (symbolp %)
                         (mkeyw %)
                         %)
                    expr))
  (let ((*tree* source)
        (*vars* #h())
        matched)
    (block nil
      (handler-bind ((html-matched ^(push (copy-hash-table *vars*) matched))
                     (finish-matching ^(return)))
        
        (do ()
            ((null *tree*))
          (when (apply #'match-expr expr)
            !!!
            (return)))))
    matched))

(defgeneric match-expr (head &rest tail)
  (:documentation
   "Match the expression of the form (head &rest tail)
    against the current *TREE*.
    If successful, returns T and match results are stored in *VARS*."))

(defmethod match-expr ((head symbol) &rest tail)
  "Match normally: (tag &rest contents)"
  (when (listp *tree*)
    (let ((saved *tree*))
      (and (or (eql head :*)
               (eql head (tag-tag (first *tree*))))
           (or (null tail)
               (loop :for (expr . exprs) :on tail :do
                 (:= *tree* (rest *tree*))
                 (cond
                   ((eql :|...| expr)
                    (if (null exprs)
                        (progn (:= *tree* nil)
                               (return t))
                        (loop :for *tree* :on *tree* :do
                          (let ((*tree* (cons :* *tree*)))
                            (when (apply #'match-expr :* exprs)
                              (return t)))
                              :finally (:= *tree* saved))))
                   ((null *tree*)
                    (:= *tree*  saved)
                    (return))
                   (t
                    (unless (let ((*tree* (first *tree*)))
                              (apply #'match-expr expr))
                      (:= *tree* saved)
                      (return))))
                     :finally (return t)))))))

(defmethod match-expr ((head list) &rest tail)
  "Match with attributes: ((tag &rest attrs) &rest content)."
  (and (listp *tree*)
       (or (eql (first head) :*)
           (eql (first head) (tag-tag (first *tree*))))
       (match-attrs (rest head))
       (apply #'match-expr (first head) tail)))

(defmethod match-expr ((head (eql :$)) &rest tail)
  "Match variables: ($ name &optional expr)."
  (when (or (single tail)
            (let ((*tree* *tree*))
              (apply #'match-expr (rest tail))))
    (:= (? *vars* (first tail)) *tree*)
    (:= *tree* nil)
    t))

(defmethod match-expr ((head (eql :$!)) &rest tail)
  (and (apply #'match-expr :$ tail)
       !!!
       (signal 'finish-matching)))

(defmethod match-expr ((head (eql :>>)) &rest tail)
  "Match by tree depth-first search: (>> tag &rest contents)."
  ;; (print (first *tree*)) (break)
  (or (apply #'match-expr tail)
      (when (listp *tree*)
        (dolist (*tree* (rest *tree*))
          (when (apply #'match-expr :>> tail)
            (return t))))
      (rutil:void *tree*)))

(defun match-attrs (attr-exprs)
  "Match current tree tag attributes against ATTR-EXPRS."
  (every #'true
         (loop :for (attr val) :on attr-exprs :by #'cddr :collect
            (and-it (tag-attrs (first *tree*))
                    (? it attr)
                    (let ((req-vals (split #\Space val))
                          (real-vals (split #\Space it)))
                      (if (member "..." req-vals :test 'string=)
                          (null (set-difference (remove "..." req-vals
                                                        :test 'string=)
                                                real-vals :test 'string=))
                          (set-equal req-vals real-vals :test 'string=)))))))


#+should-test
(deftest match-expr ()
  (should be true (let ((*tree* '(:doc))) (match-expr '*)))
  (should be null (let ((*tree* nil) (match-expr '*))))
  (should be true (let ((*tree* '(:doc))) (match-expr 'doc)))
  (should be null (let ((*tree* '(:doc))) (match-expr 'html)))
  (should be true (let ((*tree* '(:doc))) (match-expr '>> 'doc)))
  (should be true (let ((*tree* '(:doc (:html)))) (match-expr '>> html)))
  (should be true (let ((*tree* '(:doc (:fake (:html))))) (match-expr '>> html)))
  (should be null (let ((*tree* '(:doc))) (match-expr '>> 'html))))

