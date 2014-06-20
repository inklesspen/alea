;;;; rolls.lisp

(in-package #:alea)

(defun randint (n)
  (+ 1 (random n)))

(defun group (list)
  (flet ((take-same (item)
           (loop while (and list (eql (first list) item))
              collect (pop list))))
    (loop while list
       collect (take-same (first list)))))

(defun perform-ore-roll (dice-count expert-dice master-die?)
  (let ((raw-rolls (list)))
    (dotimes (i dice-count)
      (push (randint 10) raw-rolls))
    (setf raw-rolls (nreverse raw-rolls))
    (let* ((augmented (if expert-dice (append raw-rolls expert-dice) raw-rolls))
           (sorted (sort (copy-seq augmented) #'<))
           (grouped (group sorted))
           (result (mapcar #'(lambda (l) (format nil "~dx~d" (length l) (first l))) grouped))
           (result (if master-die? (nreverse (cons "MASTER" (nreverse result))) result)))
      (list :result result :explanation augmented))))

(defun perform-die-roll (dice-count die-size)
  (let ((raw-rolls (list)))
    (dotimes (i dice-count)
      (push (randint die-size) raw-rolls))
    (setf raw-rolls (nreverse raw-rolls))
    (list :result (reduce #'+ raw-rolls) :explanation (format nil "~{~a~^+~}" raw-rolls))))
