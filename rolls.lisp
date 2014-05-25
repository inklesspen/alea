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
(defgeneric response (roll))

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
   (expert-dice
    :initarg :expert-dice
    :initform nil
    :reader expert-dice)
   (master-die?
    :initarg :master-die?
    :initform nil
    :reader master-die?)
   (raw-rolls
    :initform nil)))

(defmethod initialize-instance :after ((roll ore-roll) &key)
           (with-slots (expert-dice master-die?) roll
             (when (and expert-dice master-die?)
               (error "You cannot specify both expert dice and a master die."))))

(defmethod print-object ((object ore-roll) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (dice-count expert-dice master-die?) object
      (let ((modifier (if master-die? "+M" (if expert-dice (format nil "~{+E~a~^~}" expert-dice) ""))))
        (format stream "~dd10~A" dice-count modifier)))
    (with-slots (result) object
      (when result
        (format stream ": ~a" result)))))

(defun group (list)
  (flet ((take-same (item)
           (loop while (and list (eql (first list) item))
              collect (pop list))))
    (loop while list
       collect (take-same (first list)))))

(defmethod perform-roll ((roll ore-roll))
  (with-slots (dice-count expert-dice master-die? explanation result raw-rolls) roll
    (unless raw-rolls
      (setf raw-rolls (list))
      ;;; this is probably not idiomatic; push/nreverse makes more sense
      (dotimes (i dice-count)
        (push (randint 10) raw-rolls))
      (setf raw-rolls (nreverse raw-rolls)))
    (unless explanation
      (let* ((augmented (if expert-dice (append raw-rolls expert-dice) raw-rolls))
             (sorted (sort (copy-seq augmented) #'<))
             (grouped (group sorted)))
        (setf explanation augmented)
        (setf result (mapcar #'(lambda (l) (format nil "~dx~d" (length l) (first l))) grouped))))
    roll))

(defmethod response ((roll ore-roll))
  (perform-roll roll)
  (with-slots (explanation result) roll
    (format nil "(~{~a~^ ~}): ~{~a~^ ~}" explanation result)))
