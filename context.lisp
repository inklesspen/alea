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
  ((random-state
    :initform (make-random-state t)
    :reader local-random-state)
   (known-currencies
    :allocation :class
    :initform nil
    :reader known-currencies)))

(defgeneric parse-command (context text))
(defgeneric eval-command (context place op args))

(defmethod eval-command :around ((context context) place op args)
  ;; since the contents of random-state are mutated directly,
  ;; we don't have to worry about capturing the state afterward
  ;; or setting it back into the context
  (let ((*random-state* (local-random-state context)))
    (call-next-method)))

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

(defclass tarot-context (context)
  ((deck
    :initform nil
    :accessor deck)
   (drawn
    :initform nil
    :accessor drawn)
   (chargen-in-progress
    :initform nil
    :accessor chargen-in-progress)
   (sephirot
    :initform nil
    :accessor sephirot)))

;; TODO: have an :after to pre-shuffle the deck
;; (defmethod initialize-instance :after ((session session) &key)
;;  (setf (slot-value session 'handler) (curry #'handle-clirc-message session)))
(defun shuffle-tarot-deck (context)
  (setf (deck context) (coerce (shuffle (copy-seq *tarot-cards*)) 'list))
  (setf (drawn context) nil))

(defmethod initialize-instance :after ((context tarot-context) &key)
  (shuffle-tarot-deck context))

(defmethod parse-command ((context tarot-context) text)
  ;;; shuffle
  ;;; draw
  ;;; show-drawn
  ;;; chargen
  (let ((parsed (or (handle-parse 'shuffle-deck text)
                    (handle-parse 'draw-card text)
                    (handle-parse 'tarot-chargen text))))
    (or parsed (call-next-method))))

(defmethod eval-command ((context tarot-context) place (op (eql :shuffle-deck)) args)
  (declare (ignore args))
  (shuffle-tarot-deck context)
  (setf (chargen-in-progress context) nil)
  "Shuffled!")

(defmethod eval-command ((context tarot-context) place (op (eql :draw-card)) args)
  (declare (ignore args))
  (if (chargen-in-progress context)
      (let ((card (pop (deck context)))
            (sephirah (pop (sephirot context))))
        ;; can't run out of cards while doing chargen, but can run out of sephirot
        (unless (sephirot context)
          (setf (chargen-in-progress context) nil))
        (push card (drawn context))
        (list sephirah (format nil "The card is: ~a" card)))
      (alexandria:if-let (card (pop (deck context)))
        (progn
          (push card (drawn context))
          (format nil "Your card is: ~a" card))
        "The deck is empty")))

(defmethod eval-command ((context tarot-context) place (op (eql :tarot-chargen)) args)
  (declare (ignore args))
  (setf (chargen-in-progress context) t)
  (setf (sephirot context) (copy-list *chargen-sephirot*))
  (shuffle-tarot-deck context)
  "Chargen is ready.")

(defparameter *contexts*
  (let* ((extractor (compose #'string-downcase #'first (curry #'split-sequence:split-sequence #\-) #'symbol-name #'class-name))
         (subclasses (sb-mop:class-direct-subclasses (find-class 'context)))
         (context-names (mapcar extractor subclasses)))
    (pairlis context-names subclasses)))

;;; TODO: tarot card context
;;; commands for shuffling a deck and drawing from a deck
;;; need to find a concise dataset for card meanings
;;; command for doing the tree of life chargen with 'next card' commands

;;; standard playing cards for savage worlds, hillfolk, etc
