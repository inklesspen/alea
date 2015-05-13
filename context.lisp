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

(defstruct currency synonyms symbol singular plural)
(defun format-currency (count currency)
  (if (= 1 count)
      (format nil "~d ~a" count (currency-singular currency))
      (format nil "~d ~a" count (currency-plural currency))))

(defclass context () 
  ((random-state
    :initform (make-random-state t)
    :reader local-random-state)
   (known-currencies
    :allocation :class
    :initform nil
    :reader known-currencies)
   (currency-balances
    :initform (make-hash-table :test #'equal))))

(defgeneric player-balance (context name currency))
(defmethod player-balance ((context context) name currency)
  (with-slots (currency-balances) context
    (or (gethash (cons name currency) currency-balances) 0)))

(defgeneric (setf player-balance) (value context name currency))
(defmethod (setf player-balance) (value (context context) name currency)
  (with-slots (currency-balances) context
    (setf (gethash (cons name currency) currency-balances) value)))

(defgeneric get-currency (context currency-name))
(defmethod get-currency (context currency-name)
  (find currency-name (known-currencies context)
        :key (lambda (c) (currency-synonyms c))
        :test (lambda (search synonyms) (find search synonyms :test #'string-equal))))

(defgeneric parse-command (context text))
(defgeneric eval-command (context place requester op args))

(defgeneric context-help (context))
(defgeneric command-help (context op))

;; Key is a dotted pair of context symbol and op symbol
(defparameter *examples* (make-hash-table :test #'equal))

(defmacro command-in-context (context op (&rest examples) &body body)
  `(progn
     (setf (gethash (cons (quote ,context) ,op) *examples*) (list ,@examples))
     (defmethod eval-command ((context ,context) place requester (op (eql ,op)) args)
       ,@body)))

(command-in-context context :show-help ("help")
  (format nil "~a ~{~a~^|~}"
          (context-help context)
          (remove nil
                  (mapcar (curry #'command-help context)
                          (list-commands-for-context context)))))

(defmethod context-help ((context context))
  (documentation (class-of context) t))

(defun list-commands-for-context (context)
  (mapcar
   ;; get the third specializer's object from the method
   (compose #'sb-mop:eql-specializer-object #'third #'sb-mop:method-specializers)
   ;; remove any methods with qualifiers (like :around)
   (remove-if #'sb-mop:method-qualifiers
              ;; obtain all eval-command methods for this context class
              (sb-mop:compute-applicable-methods-using-classes
               #'eval-command
               (list (class-of context) t t (class-of :op) t)))))

(defmethod command-help ((context context) op)
  nil)
  ;; (let* ((all-superclasses (sb-mop:class-precedence-list (class-of context)))
  ;;        (relevant-superclasses (iter
  ;;                                 (for klass in all-superclasses)
  ;;                                 (collect klass into klasses)
  ;;                                 (finding klasses such-that (eq (find-class 'context) klass))))
  ;;        (possible-examples (iter
  ;;                             (for klass in relevant-superclasses)
  ;;                             (collect (gethash (cons (class-name klass) op) *examples*)))))
  ;;   possible-examples))

(defmethod eval-command :around ((context context) place requester op args)
  ;; since the contents of random-state are mutated directly,
  ;; we don't have to worry about capturing the state afterward
  ;; or setting it back into the context
  (let ((*random-state* (local-random-state context)))
    (call-next-method)))

(defmethod parse-command ((context context) text)
  (or (handle-parse 'switch-context text)
      (handle-parse 'show-help text)
      (list :parse-error text)))

(defmethod eval-command ((context context) place requester (op (eql :roll)) args)
  (let* ((die-spec (first args))
         (dice-count (car die-spec))
         (die-size (cdr die-spec))
         (roll (perform-die-roll dice-count die-size)))
    roll))

(defmethod eval-command ((context context) place requester (op (eql :mod)) args)
  (list :result (first args) :explanation (format nil "~a" (first args))))

(defun wrap-explanation (explanation)
  (if (find #\+ explanation)
      (format nil "(~a)" explanation)
      explanation))

(defmethod eval-command ((context context) place requester (op (eql :plus)) args)
  (let* ((result-list (eval-command context place requester (car (first args)) (cdr (first args))))
         (result (getf result-list :result))
         (explanation (getf result-list :explanation)))
    (list :result result :explanation (format nil "+~a" (wrap-explanation explanation)))))

(defmethod eval-command ((context context) place requester (op (eql :minus)) args)
  (let* ((result-list (eval-command context place requester (car (first args)) (cdr (first args))))
         (result (* -1 (getf result-list :result)))
         (explanation (getf result-list :explanation)))
    (list :result result :explanation (format nil "-~a" (wrap-explanation explanation)))))

(command-in-context context :standard-roll ("roll 1d20" "roll 2d6+3" "roll 1d20-1d4+3")
  (let* ((sub-rolls (mapcar #'(lambda (arg) (eval-command context place requester (car arg) (cdr arg))) args))
         (results (mapcar #'(lambda (result-list) (getf result-list :result)) sub-rolls))
         (result (reduce #'+ results))
         (explanations (mapcar #'(lambda (result-list) (getf result-list :explanation)) sub-rolls))
         (explanation (format nil "~{~a~^~}" explanations))
         (explanation (if (eql #\+ (elt explanation 0)) (subseq explanation 1) explanation)))
    (format nil "~a [~a]" result explanation)))

(command-in-context context :roll-with-comment ()
  (let* ((actual-command (first args))
         (result (eval-command context place requester (car actual-command) (cdr actual-command)))
         (comment (second args)))
    (format nil "~a (comment: ~a)" result comment)))

(command-in-context context :switch-context ("switch to fate" "switch to generic" "switch wulin")
  ;; set the new context into place
  (alexandria:if-let (pair (assoc (first args) *contexts* :test #'string-equal))
    (let ((new-context (make-instance (cdr pair))))
      (setf (context place) new-context)
      (format nil "Switching to ~a" (car pair)))
    (format nil "I don't know about ~a. I only know about: ~a" (first args) (mapcar #'car *contexts*))))
    
(defclass generic-context (context) ()
  (:documentation "The generic context works for many RPGs that need to roll polyhedral dice with or without modifiers."))

(defmethod parse-command ((context generic-context) text)
  ;;; try ore-roll-with-roll, then standard-roll-with-roll, then ore-roll.
  ;;; if none work, CALL-NEXT-METHOD
  (let ((parsed (or (handle-parse 'standard-roll-with-roll text))))
    (or parsed (call-next-method))))

(defclass ore-context (context) ()
  (:documentation "The ORE context is intended for One-Roll-Engine games such as Reign or Wild Talents."))

(defmethod parse-command ((context ore-context) text)
  ;;; try ore-roll-with-roll, then standard-roll-with-roll, then ore-roll.
  ;;; if none work, CALL-NEXT-METHOD
  (let ((parsed (or (handle-parse 'ore-roll-with-roll text)
                    (handle-parse 'standard-roll-with-roll text)
                    (handle-parse 'ore-roll text))))
    (or parsed (call-next-method))))

(command-in-context ore-context :ore-roll ("roll 5d+e2" "roll 6d+m" "roll 9d+e8+e9")
  (let* ((dice-count (first args))
         (expert-dice (second args))
         (master-die? (third args))
         (roll (perform-ore-roll dice-count expert-dice master-die?)))
    (format nil "~{~a~^ ~}" (getf roll :result))))

(defclass fate-context (context)
  ((known-currencies
    :allocation :class
    :initform (list (make-currency :synonyms '("fate points" "fate point" "fp") :symbol :fate-point :singular "fate point" :plural "fate points"))
    :reader known-currencies))
  (:documentation "The FATE context is for games which use FUDGE/FATE dice and FATE points."))

(defmethod parse-command ((context fate-context) text)
  ;;; fate-roll-with-roll
  ;;; currency
  ;;; standard-roll-with-roll
  ;;; fate-roll
  ;;; if none work, CALL-NEXT-METHOD
  (let ((parsed (or (handle-parse 'fate-roll-with-roll text)
                    (handle-parse 'currency-command text)
                    (handle-parse 'standard-roll-with-roll text)
                    (handle-parse 'fate-roll text))))
    (or parsed (call-next-method))))

(command-in-context fate-context :fate-roll ("roll 4dF+2" "roll 4dF-3" "roll 4dF")
  (let* ((dice-count (first args))
         (mod-sign (case (second args) ((:plus) 1) ((:minus) -1) (otherwise 0)))
         (mod (* mod-sign (or (third args) 0)))
         (roll (perform-fate-roll dice-count mod)))
    (format nil "~a (~a)" (getf roll :result) (getf roll :explanation))))

;; This example belongs at the fate level, but the implementation probably belongs at the base context level. What to do, what to do...

;; grant is a synonym for give; they both pay from the infinite bank.
;; with pay, you cannot pay more than you already have
(command-in-context context :pay-currency ("pay 5 fate points" "pay sam 5 fp" "pay 5 fp to sam")
  (let* ((payment (first args))
         (recipient (second args))
         (amount (second payment))
         (currency-name (third payment))
         (currency (get-currency context currency-name)))
    (if currency
        (let ((current-balance (player-balance context requester (currency-symbol currency))))
          (if (< current-balance amount)
              (format nil "You cannot pay ~a; you have only ~a." (format-currency amount currency) current-balance)
              (let ((new-balance (decf (player-balance context requester (currency-symbol currency)) amount)))
                (if (eql :bank recipient)
                    (format nil "You now have ~a." (format-currency new-balance currency))
                    (let ((recipient-new-balance (incf (player-balance context recipient (currency-symbol currency)) amount)))
                      (format nil "You now have ~a. ~a now has ~a." (format-currency new-balance currency) recipient (format-currency recipient-new-balance currency)))))))
        (format nil "I don't recognize \"~a\" as a currency." currency-name))))

(command-in-context context :grant-currency ("grant 1 fate point to sam" "grant sam 1 fp")
  (let* ((payment (first args))
         (recipient (second args))
         (amount (second payment))
         (currency-name (third payment))
         (currency (get-currency context currency-name)))
    (if currency
        (let ((new-balance (incf (player-balance context recipient (currency-symbol currency)) amount)))
          (format nil "~a now has ~a." recipient (format-currency new-balance currency)))
        (format nil "I don't recognize \"~a\" as a currency." currency-name))))

(command-in-context context :balance ("balance" "balance all" "balance sam")
  (let* ((target (first args)))
    (case target
      ((:me) "You!")
      ((:all) "Everyone!")
      (otherwise (format nil "~a!" target)))))

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
    :accessor sephirot))
  (:documentation "The tarot context supports shuffling tarot decks and dealing cards, as well as tarot-based character generation."))

(defun shuffle-tarot-deck (context)
  (setf (deck context) (coerce (shuffle (copy-seq *tarot-cards*)) 'list))
  (setf (drawn context) nil))

(defmethod initialize-instance :after ((context tarot-context) &key)
  ;; We have to wrap it like this because this isn't subject to the :around for eval.
  (let ((*random-state* (local-random-state context)))
    (shuffle-tarot-deck context)))

(defmethod parse-command ((context tarot-context) text)
  ;;; shuffle
  ;;; draw
  ;;; show-drawn
  ;;; chargen
  (let ((parsed (or (handle-parse 'shuffle-deck text)
                    (handle-parse 'draw-card text)
                    (handle-parse 'tarot-chargen text))))
    (or parsed (call-next-method))))

(command-in-context tarot-context :shuffle-deck ("shuffle" "shuffle deck")
  (declare (ignore args))
  (shuffle-tarot-deck context)
  (setf (chargen-in-progress context) nil)
  "Shuffled!")

(command-in-context tarot-context :draw-card ("draw" "draw card")
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

(command-in-context tarot-context :tarot-chargen ("chargen")
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
