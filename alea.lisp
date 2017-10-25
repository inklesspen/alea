;;;; alea.lisp

(in-package #:alea)

;;; "alea" goes here. Hacks and glory await!

;;; interaction:
;;; user types text
;;; parsers (provided by context) transform text into nil or an AST
;;; context transforms AST into a Displayable
;;; alea renders Displayable to the appropriate channel


;;; one campaign per discord server
;;; use UUID-5 or UUID-4 for identifiers
;;; provide a url where all public rolls made in a server can be viewed
