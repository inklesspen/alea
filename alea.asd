;;;; alea.asd

(asdf:defsystem #:alea
  :serial t
  :description "Describe alea here"
  :author "inklesspen"
  :license "GPL"
  :depends-on (#:cl-irc
               #:alexandria
               #:esrap
               #:iterate)
  :components ((:file "package")
               (:file "context")
               (:file "rolls")
               (:file "parsing")
               (:file "tarot")
               (:file "alea")))

