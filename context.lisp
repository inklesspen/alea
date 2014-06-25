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
(defgeneric eval-command (context place op args))

(defmethod parse-command ((context context) text)
  (or (handle-parse 'switch-context text)
      (list :parse-error text)))

(defmethod eval-command ((context context) place (op (eql :roll)) args)
  (let* ((die-spec (first args))
         (dice-count (car die-spec))
         (die-size (cdr die-spec))
         (roll (perform-die-roll dice-count die-size)))
    roll))

(defmethod eval-command ((context context) place (op (eql :mod)) args)
  (list :result (first args) :explanation (format nil "~a" (first args))))

(defun wrap-explanation (explanation)
  (if (find #\+ explanation)
      (format nil "(~a)" explanation)
      explanation))

(defmethod eval-command ((context context) place (op (eql :plus)) args)
  (let* ((result-list (eval-command context place (car (first args)) (cdr (first args))))
         (result (getf result-list :result))
         (explanation (getf result-list :explanation)))
    (list :result result :explanation (format nil "+~a" (wrap-explanation explanation)))))

(defmethod eval-command ((context context) place (op (eql :minus)) args)
  (let* ((result-list (eval-command context place (car (first args)) (cdr (first args))))
         (result (* -1 (getf result-list :result)))
         (explanation (getf result-list :explanation)))
    (list :result result :explanation (format nil "-~a" (wrap-explanation explanation)))))

(defmethod eval-command ((context context) place (op (eql :standard-roll)) args)
  (let* ((sub-rolls (mapcar #'(lambda (arg) (eval-command context place (car arg) (cdr arg))) args))
         (results (mapcar #'(lambda (result-list) (getf result-list :result)) sub-rolls))
         (result (reduce #'+ results))
         (explanations (mapcar #'(lambda (result-list) (getf result-list :explanation)) sub-rolls))
         (explanation (format nil "~{~a~^~}" explanations))
         (explanation (if (eql #\+ (elt explanation 0)) (subseq explanation 1) explanation)))
    (format nil "~a [~a]" result explanation)))

(defmethod eval-command ((context context) place (op (eql :roll-with-comment)) args)
  (let* ((actual-command (first args))
         (result (eval-command context place (car actual-command) (cdr actual-command)))
         (comment (second args)))
    (format nil "~a (comment: ~a)" result comment)))

(defmethod eval-command ((context context) place (op (eql :switch-context)) args)
  ;; set the new context into place
  (alexandria:if-let (pair (assoc (first args) *contexts* :test #'string-equal))
    (let ((new-context (make-instance (cdr pair))))
      (setf (context place) new-context)
      (format nil "Switching to ~a" (car pair)))
    (format nil "I don't know about ~a. I only know about: ~a" (first args) (mapcar #'car *contexts*))))
    
(defclass generic-context (context) ())

(defmethod parse-command ((context generic-context) text)
  ;;; try ore-roll-with-roll, then standard-roll-with-roll, then ore-roll.
  ;;; if none work, CALL-NEXT-METHOD
  (let ((parsed (or (handle-parse 'standard-roll-with-roll text))))
    (or parsed (call-next-method))))

(defclass ore-context (context) ())

(defmethod parse-command ((context ore-context) text)
  ;;; try ore-roll-with-roll, then standard-roll-with-roll, then ore-roll.
  ;;; if none work, CALL-NEXT-METHOD
  (let ((parsed (or (handle-parse 'ore-roll-with-roll text)
                    (handle-parse 'standard-roll-with-roll text)
                    (handle-parse 'ore-roll text))))
    (or parsed (call-next-method))))

(defmethod eval-command ((context context) place (op (eql :ore-roll)) args)
  (let* ((dice-count (first args))
         (expert-dice (second args))
         (master-die? (third args))
         (roll (perform-ore-roll dice-count expert-dice master-die?)))
    (format nil "~{~a~^ ~}" (getf roll :result))))

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

(defmethod eval-command ((context context) place (op (eql :fate-roll)) args)
  (let* ((dice-count (first args))
         (mod-sign (case (second args) ((:plus) 1) ((:minus) -1) (otherwise 0)))
         (mod (* mod-sign (or (third args) 0)))
         (roll (perform-fate-roll dice-count mod)))
    (format nil "~a (~a)" (getf roll :result) (getf roll :explanation))))

(defparameter *contexts*
  (let* ((extractor (compose #'string-downcase #'first (curry #'split-sequence:split-sequence #\-) #'symbol-name #'class-name))
         (subclasses (sb-mop:class-direct-subclasses (find-class 'context)))
         (context-names (mapcar extractor subclasses)))
    (pairlis context-names subclasses)))
