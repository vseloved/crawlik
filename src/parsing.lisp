;;; CRAWLIK HTML parser
;;; see LICENSE file for permissions

(in-package #:crawlik)
(named-readtables:in-readtable rutilsx-readtable)


(defstruct (tag (:print-function (lambda (obj stream depth)
                                   (declare (ignore depth))
                                   (format stream "<~:@(~A~)~{ ~A~}>"
                                           @obj.tag
                                           (when @obj.attrs
                                             (mapcar #`(fmt "~(~A~)='~A'"
                                                            (lt %) (rt %))
                                                     (ht->pairs @obj.attrs)))))))
  tag attrs)


(defclass dumb-html5-parser (sax:sax-parser-mixin)
  ((stack :accessor sax-stack
          :initform (list (vec (make-tag :tag :DOC)))))
  (:documentation
   "Primitive HTML5 parser."))

(defmethod sax:start-element ((sax dumb-html5-parser)
                              namespace-uri local-name qname attrs)
  (let ((cur (vec (make-tag
                   :tag (mkeyw local-name)
                   :attrs (when-it (mapcar #`(pair (mkeyw (sax:attribute-local-name %))
                                                   (sax:attribute-value %))
                                           attrs)
                            (pairs->ht it))))))
    (vector-push-extend cur (first @sax.stack))
    (push cur @sax.stack)
    @sax.stack))

(defmethod sax:end-element ((sax dumb-html5-parser)
                            namespace-uri local-name qname)
  (pop @sax.stack))

(defmethod sax:characters ((sax dumb-html5-parser) data)
  (let ((trimmed (string-trim '(#\Newline #\Return #\Linefeed #\Tab #\Space)
                              data)))
    (unless (blankp trimmed)
      (vector-push-extend trimmed (first @sax.stack)))))

(defmethod sax:end-document ((sax dumb-html5-parser))
  (vec->list-tree (pop @sax.stack)))

(defmethod sax:start-document ((sax dumb-html5-parser)))
(defmethod sax:start-dtd ((handler dumb-html5-parser) name publicid systemid))
(defmethod sax:end-dtd ((handler dumb-html5-parser)))
(defmethod sax::dtd ((handler dumb-html5-parser) dtd))
(defmethod sax:internal-entity-declaration
    ((handler dumb-html5-parser) kind name value))
(defmethod sax:element-declaration ((handler dumb-html5-parser) name model))
(defmethod sax:attribute-declaration
    ((handler dumb-html5-parser) element-name attribute-name type default))
(defmethod sax:entity-resolver ((handler dumb-html5-parser) resolver))
(defmethod sax:start-prefix-mapping ((handler dumb-html5-parser) prefix uri))
(defmethod sax:end-prefix-mapping ((handler dumb-html5-parser) prefix))
(defmethod sax:comment ((handler dumb-html5-parser) data))
(defmethod sax:start-cdata ((handler dumb-html5-parser)))
(defmethod sax:end-cdata ((handler dumb-html5-parser)))

(defun dtd-resolver (pubid sysid)
  (declare (ignore sysid pubid))
  (open (asdf:system-relative-pathname :crawlik "dtds/xhtml1-transitional.dtd")
        :element-type '(unsigned-byte 8)))

(defun parse-dirty-xml (source)
  (handler-bind ((cxml:well-formedness-violation
                   (lambda (e)
                     (declare (ignore e))
                     (when-it (or (find-restart 'cxml::ignore-namespace)
                                  (find-restart 'cxml::ignore-double-attr)
                                  (find-restart 'cxml::close-end-tag)
                                  (find-restart 'cxml::treat-<-as-lt)
                                  (find-restart 'cxml::treat-single-&-as-is))
                       (invoke-restart it)))))
    (cxml:parse (-> source
                    (re:regex-replace "<![^>]+>" % "")
                    (re:regex-replace "<html[^>]+>" % "<html>"))
                (make 'dumb-html5-parser)
                :dtd (cxml:make-extid
                      nil
                      (puri:uri (strcat "file://"
                                        (asdf:system-relative-pathname
                                         :crawlik "dtds/xhtml1-transitional.dtd"))))
                                                            
                :entity-resolver 'dtd-resolver)))


;;; utils

(defun vec->list-tree (vec-tree)
  (map 'list #`(if (and (vectorp %)
                        (plusp (length %))
                        (tag-p (? % 0)))
                   (vec->list-tree %)
                   %)
       vec-tree))
