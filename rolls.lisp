;;;; rolls.lisp

(in-package #:alea)

(defun randint (n)
  (+ 1 (random n)))

(defun shuffle (seq)
  ;; Knuth shuffle (in place)
  (let ((n (length seq)))
    (dotimes (i n seq)
      (rotatef (elt seq i) (elt seq (+ i (random (- n i))))))))

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


(defun perform-fate-roll (dice-count mod)
  (let ((raw-rolls (list)))
    (dotimes (i dice-count)
      (push (- (randint 3) 2) raw-rolls))
    (setf raw-rolls (sort raw-rolls #'<))
    (let* ((explanation (format nil "[~{~a~^ ~}]" raw-rolls))
           (explanation (if (eql 0 mod)
                            explanation
                            (format nil "~a ~@d" explanation mod))))
      (list :result (reduce #'+ (cons mod raw-rolls)) :explanation explanation))))


(defun perform-exploding-roll (die-size)
  (let ((raw-rolls (list)))
    (loop
       (when (and raw-rolls (not (eq 6 (first raw-rolls))))
         (return))
       (push (randint die-size) raw-rolls))
    (setf raw-rolls (nreverse raw-rolls))
    (list :result (reduce #'+ raw-rolls) :explanation (format nil "~{~a~^+~}" raw-rolls))))
    
