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

(defgeneric handle-command (context text))
(defmethod handle-command ((context context) text)
  (format nil "I don't know how to handle: ~a" text))

(defclass ore-context (context) ())
(defmethod handle-command ((context ore-context) text)
  ;;; try ore-roll-with-roll, then standard-roll-with-roll, then ore-roll.
  ;;; if none work, CALL-NEXT-METHOD
  (let ((parsed (or (handle-parse 'ore-roll-with-roll text)
                    (handle-parse 'standard-roll-with-roll text)
                    (handle-parse 'ore-roll text))))
    (if parsed
        (format nil "~a~@[ (~a)~]" (response (parsed-command parsed)) (parsed-comment parsed))
        (call-next-method))))
