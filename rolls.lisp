;;;; rolls.lisp

(in-package #:alea)

(defun randint (n)
  (+ 1 (random n)))

(defclass roll ()
  ((result
    :initform nil
    :accessor result)
   (explanation
    :initform nil
    :accessor explanation)))

(defgeneric perform-roll (roll))

(defclass standard-roll (roll)
  ((dice-count
    :initarg :dice-count
    :initform (error "Must provide dice count")
    :reader dice-count)
   (die-size
    :initarg :die-size
    :initform (error "Must provide die size")
    :reader die-size)
   (modifier
    :initarg :modifier
    :initform 0
    :reader modifier)))

(defclass ore-roll (roll)
  ((dice-count
    :initarg :dice-count
    :initform (error "Must provide dice count")
    :reader dice-count)
   (expert-die
    :initarg :expert-die
    :initform nil
    :reader expert-die)
   (master-die?
    :initarg :master-die?
    :initform nil
    :reader master-die?)
   (raw-rolls
    :initform nil)))

(defmethod print-object ((object ore-roll) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (dice-count expert-die master-die?) object
      (let ((modifier (if master-die? "+M" (if expert-die (format nil "+E~d" expert-die) ""))))
        (format stream "~dd10~A" dice-count modifier)))
    (with-slots (result) object
      (when result
        (format stream ": ~a" result)))))

(defmethod perform-roll ((roll ore-roll))
  (with-slots (dice-count expert-die master-die? explanation result raw-rolls) roll
    (unless raw-rolls
      (setf raw-rolls (list))
      (dotimes (i dice-count)
        (setf raw-rolls (cons (randint 10) raw-rolls)))
      (setf raw-rolls (reverse raw-rolls)))
    (unless explanation
      (let* ((augmented (if expert-die (cons expert-die raw-rolls) raw-rolls))
             (sorted (sort (copy-seq augmented) #'<)))
        (setf explanation (format nil "~{~a~^ ~}" sorted))
        ;;; make pairs for result sometime
        (setf result explanation)
        ))
    roll))
