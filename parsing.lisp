;;;; parsing.lisp

(in-package #:alea)

;;; defrule does not share a namespace with functions or variables

(esrap:defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(esrap:defrule alphanumeric (alphanumericp character))

(esrap:defrule word (+ alphanumeric)
  (:function esrap:text))

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

(esrap:defrule ore-diesize (and (esrap:~ "d")
                                (or "10"
                                    (esrap:! alphanumeric)))
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

;;;;; Currency

;; We need a not-followed-by to keep the second word in the name from being 'to'
;; since that would interfere with the pay-to rule
(esrap:defrule currency-name (and word (esrap:? (and #\space (esrap:! "to") word)))
  (:function esrap:text))

(esrap:defrule currency (and integer (esrap:? whitespace) currency-name)
  (:function (lambda (parsed)
               (list :currency (first parsed) (third parsed)))))

(esrap:defrule pay-basic (and "pay" whitespace currency)
  (:function (lambda (parsed)
               (list :pay-currency (third parsed) :bank))))

(esrap:defrule pay-name (and "pay" whitespace word whitespace currency)
  (:function (lambda (parsed)
               (list :pay-currency (fifth parsed) (third parsed)))))

(esrap:defrule pay-to (and "pay" whitespace currency whitespace "to" whitespace word)
  (:function (lambda (parsed)
               (list :pay-currency (third parsed) (seventh parsed)))))

(esrap:defrule pay-currency (or pay-to pay-name pay-basic))

(esrap:defrule grant-name (and "grant" whitespace word whitespace currency)
  (:function (lambda (parsed)
               (list :grant-currency (fifth parsed) (third parsed)))))

(esrap:defrule grant-to (and "grant" whitespace currency whitespace "to" whitespace word)
  (:function (lambda (parsed)
               (list :grant-currency (third parsed) (seventh parsed)))))

(esrap:defrule grant-currency (or grant-to grant-name))

(esrap:defrule give-name (and "give" whitespace word whitespace currency)
  (:function (lambda (parsed)
               (list :give-currency (fifth parsed) (third parsed)))))

(esrap:defrule give-to (and "give" whitespace currency whitespace "to" whitespace word)
  (:function (lambda (parsed)
               (list :give-currency (third parsed) (seventh parsed)))))

(esrap:defrule give-currency (or give-to give-name))

;; balance
;; balance all
;; balance landon

(esrap:defrule balance (and "balance" (esrap:? (and whitespace word)))
  (:function (lambda (parsed)
               (let* ((target (or (cadadr parsed) :me))
                      (target (if (string-equal "all" target) :all target)))
                 (list :balance target)))))

;;;;; Context

(esrap:defrule switch-context (and "context" whitespace (+ alphanumeric))
  (:function (lambda (parsed)
               (list :switch-context (esrap:text (third parsed))))))

(esrap:defrule shuffle-deck (and "shuffle" (esrap:? (and whitespace "deck")))
  (:function (lambda (parsed)
               (declare (ignore parsed))
               (list :shuffle-deck))))

(esrap:defrule draw-card (and "draw" (esrap:? (and whitespace "card")))
  (:function (lambda (parsed)
               (declare (ignore parsed))
               (list :draw-card))))

(defun handle-parse (rule text)
  (multiple-value-bind (raw-parse next-char success?)
      (handler-case (esrap:parse rule text :junk-allowed t)
        (esrap:esrap-error () nil))
    (when success?
      (let ((comment (when next-char (string-trim
                                      '(#\space #\tab #\newline)
                                      (subseq text next-char)))))
        (if comment
            (if (search "ROLL" (string (first raw-parse)))
                `(:roll-with-comment ,raw-parse ,comment)
                ;; Return nil if the command isn't a ROLL
                nil)
            raw-parse)))))
