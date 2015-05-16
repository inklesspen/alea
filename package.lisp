;;;; package.lisp

(in-package "COMMON-LISP-USER")
(defpackage #:alea
  (:use #:cl #:iterate)
  (:import-from :alexandria :curry :compose)
  (:export :launch))

