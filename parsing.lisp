;;;; parsing.lisp

(in-package #:alea)

;;; defrule does not share a namespace with functions or variables

(esrap:defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(esrap:defrule integer (+ (digit-char-p character))
  (:function (lambda (parsed) (parse-integer (esrap:text parsed)))))

(esrap:defrule diesize (and (esrap:~ "d") integer))

(esrap:defrule diespec (and integer diesize)
  (:function (lambda (parsed)
               (let ((dice-count (first parsed))
                     (die-size (second (second parsed))))
                 (list :roll (cons dice-count die-size))))))

(esrap:defrule standard-mod integer
  (:function (lambda (parsed)
               (list :mod parsed))))

(esrap:defrule standard-roll-part (or diespec standard-mod))

(esrap:defrule leading-roll-part diespec
  (:function (lambda (parsed)
               (list :plus parsed))))

(esrap:defrule following-roll-part (and (or "+" "-") standard-roll-part)
  (:function (lambda (parsed)
               (let ((keyword (if (equal (first parsed) "+") :plus :minus)))
                 (list keyword (second parsed))))))

(esrap:defrule standard-roll (and leading-roll-part (* following-roll-part))
  (:function (lambda (parsed)
               (cons :standard-roll (cons (first parsed) (second parsed))))))

(esrap:defrule ore-mod (and (or "+" whitespace) (or (and (esrap:~ "e") integer) (esrap:~ "m")))
  (:function second))

(esrap:defrule ore-diesize (and (esrap:~ "d") (or "10"
                                                  (esrap:! (alphanumericp character))))
  (:constant nil))

(esrap:defrule ore-roll (and integer ore-diesize (esrap:* ore-mod))
  (:function (lambda (parsed)
               (let*
                   ((dice-count (first parsed))
                    (raw-mods (let ((x (third parsed))) (if (listp x) x (list))))
                    (expert-dice (mapcan #'(lambda (e)
                                             (when (and (listp e) (equal "e" (first e)))
                                               (list (second e)))) raw-mods))
                    (master-die? (some #'(lambda (x) (equal "m" x)) raw-mods)))
                 `(:ore-roll ,dice-count ,expert-dice ,master-die?)))))

(esrap:defrule ore-roll-with-roll (and "roll" whitespace ore-roll)
  (:function third))

(esrap:defrule standard-roll-with-roll (and "roll" whitespace standard-roll)
  (:function third))

(esrap:defrule fate-base-roll (and "4" (esrap:~ "d") (esrap:~ "f"))
  (:constant 4))

(esrap:defrule fate-mod (and (or "+" "-") integer)
  (:function (lambda (parsed)
               (let ((keyword (if (equal (first parsed) "+") :plus :minus)))
                 (list keyword (second parsed))))))

(esrap:defrule fate-roll (and fate-base-roll (esrap:? fate-mod))
  (:function (lambda (parsed)
               (cons :fate-roll (cons (first parsed) (second parsed))))))

(esrap:defrule fate-roll-with-roll (and "roll" whitespace fate-roll)
  (:function third))

(defun handle-parse (rule text)
  (multiple-value-bind (raw-parse next-char success?)
      (handler-case (esrap:parse rule text :junk-allowed t)
        (esrap:esrap-error () nil))
    (when success?
      (let ((comment (when next-char (string-trim
                                      '(#\space #\tab #\newline)
                                      (subseq text next-char)))))
        (if comment
            `(:roll-with-comment ,raw-parse ,comment)
            raw-parse)))))
