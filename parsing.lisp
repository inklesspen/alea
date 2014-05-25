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

;;; TODO: support syntax like 1d8+2d6-1d4
(esrap:defrule standard-roll (and diespec (esrap:? standard-mod)))

(esrap:defrule ore-mod (and (or "+" whitespace) (or (and (esrap:~ "e") integer) (esrap:~ "m")))
  (:function second))

(esrap:defrule ore-diesize (and (esrap:~ "d") (esrap:? "10"))
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
                 (make-instance 'ore-roll
                                :dice-count dice-count
                                :expert-dice expert-dice
                                :master-die? master-die?)))))

(esrap:defrule ore-roll-with-roll (and "roll" whitespace ore-roll)
  (:function third))

(esrap:defrule standard-roll-with-roll (and "roll" whitespace standard-roll)
  (:function third))

(defstruct parsed command comment)

(defun handle-parse (rule text)
  (multiple-value-bind (raw-parse next-char success?)
      (handler-case (esrap:parse rule text :junk-allowed t)
        (esrap:esrap-error () nil))
    (declare (ignore success?))
    (let ((comment (when next-char (string-trim
                                    '(#\space #\tab #\newline)
                                    (subseq text next-char)))))
      (make-parsed :command raw-parse :comment comment))))

