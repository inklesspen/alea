;;;; parsing.lisp

(in-package #:alea)

;;; (cl-ppcre:scan-to-strings (cl-ppcre:create-scanner "\\A(\\d{1,3})d(\\+(E\\d{1,2}|M))?\\Z") "5d+E8")

(esrap:defrule integer (+ (digit-char-p character))
  (:function (lambda (parsed) (parse-integer (esrap:text parsed)))))

(esrap:defrule diesize (and (esrap:~ "d") integer))

(esrap:defrule diespec (and integer diesize))

(esrap:defrule ore-mod (and "+" (or (and (esrap:~ "e") integer) (esrap:~ "m"))))

(esrap:defrule ore-diesize (and (esrap:~ "d") (esrap:? "10"))
  (:constant nil))

(esrap:defrule ore-roll (and integer ore-diesize (esrap:? ore-mod)))

