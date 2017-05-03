;;; CRAWLIK system definition
;;; see LICENSE file for permissions

(asdf:defsystem #:crawlik
  :name "Crawlik"
  :version (:read-file-line "version.txt")
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "3-clause MIT licence"
  :description
  "A declarative crawler."
  :depends-on (#:rutilsx #:drakma #:cl-ppcre #:cxml #:eager-future2
                         #+dev #:should-test)
  :components
  ((:module #:src
    :serial t
    :components ((:file "package")
                 (:file "cxml-patches")
                 (:file "parsing")
                 (:file "lang")
                 (:file "core")))))
