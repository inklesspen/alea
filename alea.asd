;;;; alea.asd

(asdf:defsystem #:alea
  :serial t
  :description "Describe alea here"
  :author "inklesspen"
  :license "GPL"
  :depends-on (#:cl-irc
               #:alexandria)
  :components ((:file "package")
               (:file "alea")))

