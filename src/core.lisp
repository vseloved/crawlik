;;; CRAWLIK main framework
;;; see LICENSE file for permissions

(in-package #:crawlik)
(named-readtables:in-readtable rutilsx-readtable)

(defgeneric scrape (site source)
  (:documentation
   "Extract data from SOURCE according to the logic of a particular SITE.
    The methods of this function should return the data as first value
    and, optionally, the link to the next page to crawl as the second one."))

(defgeneric crawl (site)
  (:documentation
   "Process all the pages of the SITE according to some rule in "))

(defmethod crawl (site)
  (let ((raw (drakma:http-request @site.url))
        rez)
    (loop (with ((content next (scrape site raw)))
            (push content rez)
            (when (or next @site.next-fn)
              (:= raw (drakma:http-request (or next (call @site.next-fn)))))))
    (reverse rez)))
