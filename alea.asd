;;;; alea.asd

(asdf:defsystem #:alea
  :serial t
  :description "Describe alea here"
  :author "inklesspen"
  :license "GPL"
  :depends-on (#:cl-irc
               #:alexandria
               #:esrap)
  :components ((:file "package")
               (:file "parsing")
               (:file "alea")))

