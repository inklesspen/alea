;;;; context.lisp

(in-package #:alea)

;;; hooks: listen for commands

;;; !roll something
;;; Alea: roll something
;;; ! syntax doesn't squawk on errors; nick-syntax does

;;; let's call them 'contexts'. they can be instantiated, in case they need to store state

;;; each context has a set of commands. it looks at the incoming
;;; message and tries to pick a suitable command. 'standard' context
;;; would just be 'roll <some dice>', while 'GSS' context would be all
;;; about gaining or losing the currencies the context is responsible
;;; for looking at the message and dispatching to the command. so
;;; ORE's context would first try to match against 'roll' with a ORE
;;; style reporting for d10s. if that doesn't work (say it says
;;; 2d12+5) it will match against a generic 'roll'

(defclass context () 
  ((known-currencies
    :allocation :class
    :initform nil
    :reader known-currencies)))

(defgeneric parse-command (context text))
(defgeneric eval-command (context command))

(defmethod parse-command ((context context) text)
  (or (handle-parse 'standard-roll-with-roll text)
      (list :parse-error text)))

(defmethod eval-command ((context context) (command cons))
  (let ((op (car command)) (args (cdr command))
        (wrap (lambda (explanation)
                (if (find #\+ explanation)
                    (format nil "(~a)" explanation)
                    explanation))))
    (case op
      ((:roll)
       (let* ((die-spec (first args))
              (dice-count (car die-spec))
              (die-size (cdr die-spec))
              (roll (perform-die-roll dice-count die-size)))
         roll))
      ((:mod)
       (list :result (first args) :explanation (format nil "~a" (first args))))
      ((:plus)
       (let* ((result-list (eval-command context (first args)))
              (result (getf result-list :result))
              (explanation (getf result-list :explanation)))
         (list :result result :explanation (format nil "+~a" (funcall wrap explanation)))))
      ((:minus)
       (let* ((result-list (eval-command context (first args)))
              (result (* -1 (getf result-list :result)))
              (explanation (getf result-list :explanation)))
         (list :result result :explanation (format nil "-~a" (funcall wrap explanation)))))
      ((:standard-roll)
       (let* ((sub-rolls (mapcar #'(lambda (arg) (eval-command context arg)) args))
              (results (mapcar #'(lambda (result-list) (getf result-list :result)) sub-rolls))
              (result (reduce #'+ results))
              (explanations (mapcar #'(lambda (result-list) (getf result-list :explanation)) sub-rolls))
              (explanation (format nil "~{~a~^~}" explanations))
              (explanation (if (eql #\+ (elt explanation 0)) (subseq explanation 1) explanation)))
         (format nil "~a [~a]" result explanation)))
      ((:roll-with-comment)
       (format nil "~a (comment: ~a)" (eval-command context (first args)) (second args)))
      (otherwise (call-next-method)))))

(defclass generic-context (context) ())

(defclass ore-context (context) ())

(defmethod parse-command ((context ore-context) text)
  ;;; try ore-roll-with-roll, then standard-roll-with-roll, then ore-roll.
  ;;; if none work, CALL-NEXT-METHOD
  (let ((parsed (or (handle-parse 'ore-roll-with-roll text)
                    (handle-parse 'standard-roll-with-roll text)
                    (handle-parse 'ore-roll text))))
    (or parsed (call-next-method))))

(defmethod eval-command ((context ore-context) (command cons))
  (let ((op (car command)) (args (cdr command)))
    (case op
      ((:ore-roll)
       (let* ((dice-count (first args))
              (expert-dice (second args))
              (master-die? (third args))
              (roll (perform-ore-roll dice-count expert-dice master-die?)))
         (format nil "~{~a~^ ~}" (getf roll :result))))
      (otherwise (call-next-method)))))

(defclass fate-context (context)
  ((known-currencies
    :allocation :class
    :initform '(("fate point" . :fate-point) ("fp" . :fate-point))
    :reader known-currencies)))

(defmethod parse-command ((context fate-context) text)
  ;;; fate-roll-with-roll
  ;;; currency
  ;;; standard-roll-with-roll
  ;;; fate-roll
  ;;; if none work, CALL-NEXT-METHOD
  (let ((parsed (or (handle-parse 'fate-roll-with-roll text)
                    (handle-parse 'standard-roll-with-roll text)
                    (handle-parse 'fate-roll text))))
    (or parsed (call-next-method))))

(defmethod eval-command ((context fate-context) (command cons))
  (let ((op (car command)) (args (cdr command)))
    (case op
      ((:fate-roll)
       (let* ((dice-count (first args))
              (mod-sign (case (second args) ((:plus) 1) ((:minus) -1) (otherwise 0)))
              (mod (* mod-sign (or (third args) 0)))
              (roll (perform-fate-roll dice-count mod)))
         (format nil "~a (~a)" (getf roll :result) (getf roll :explanation))))
      (otherwise (call-next-method)))))
