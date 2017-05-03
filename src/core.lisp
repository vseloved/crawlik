;;; CRAWLIK main framework
;;; see LICENSE file for permissions

(in-package #:crawlik)
(named-readtables:in-readtable rutilsx-readtable)

(defgeneric srape (site source)
  (:documentation
   "Extract data from SOURCE according to the logic of a particular SITE.
    The methods of this function should return the data as first value
    and, optionally, the link to the next page to crawl as the second one."))

(defgeneric crawl (site &optional url next-fn)
  (:documentation
   ""))

(defmethod crawl (site &optional url next-fn)
  (let ((raw (drakma:http-request url))
        rez)
    (loop (with ((content next (scrape site raw)))
            (push content rez)
            (when (or next next-fn)
              (:= raw (drakma:http-request (or next (call next-fn)))))))
    (reverse rez)))
