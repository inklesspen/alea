;;;; parsing.lisp

(in-package #:alea)

;;; defrule does not share a namespace with functions or variables

(esrap:defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(esrap:defrule integer (+ (digit-char-p character))
  (:function (lambda (parsed) (parse-integer (esrap:text parsed)))))

(esrap:defrule diesize (and (esrap:~ "d") integer))

(esrap:defrule diespec (and integer diesize))

(esrap:defrule standard-mod (and (or "+" "-") integer))

;;; TODO: support syntax like 1d8+2d6-1d4+10 or even 1d20+5+3d6
(esrap:defrule standard-roll (and diespec (esrap:? standard-mod))
  (:function (lambda (parsed)
               (let* ((dice-construct (first parsed))
                      (dice-count (first dice-construct))
                      (die-size (second (second dice-construct)))
                      (mod-construct (second parsed))
                      (mul (if mod-construct (if (equal (first mod-construct) "+") 1 -1) 0))
                      (raw-mod (if mod-construct (second mod-construct) 0))
                      (modifier (when mod-construct (* mul raw-mod))))
               `(:standard-roll (,dice-count . ,die-size) ,modifier)))))

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
