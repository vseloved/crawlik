;;; CRAWLIK package definition
;;; see LICENSE file for permissions

(rename-package :cl-ppcre :cl-ppcre '(:ppcre :re))
(rename-package :eager-future2 :eager-future2 '(:fut))
(:= drakma:*drakma-default-external-format* :utf-8)

(defpackage :crawlik
  (:use :cl :rutilsx)
  (:export #:crawl
           #:scrape

           #:parse-dirty-xml
           #:match-html
           #:match-expr
           #:match-attrs))
