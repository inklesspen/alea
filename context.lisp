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

(defclass context () ())

(defgeneric parse-command (context text))
(defgeneric eval-command (context command))

(defmethod parse-command ((context context) text)
  (or (handle-parse 'standard-roll-with-roll text)
      (list :parse-error text)))

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
      ((:standard-roll)
       (format nil "~a" command))
      ((:roll-with-comment)
       (format nil "~a (comment: ~a)" (eval-command context (first args)) (second args)))
      (otherwise (call-next-method)))))

  
