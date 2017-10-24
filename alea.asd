;;;; alea.asd

(asdf:defsystem #:alea
  :description "Describe alea here"
  :author "Rose Davidson"
  :license "GPL"
  :depends-on (#:websocket-driver-server
               #:hunchensocket
               #:alexandria
               #:esrap
               #:iterate)
  :serial t
  :components ((:file "package")
               (:file "alea")))

