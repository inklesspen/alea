(in-package #:alea)

(defun launch-inner (nickname host port channel)
  (let ((session (make-instance 'session
                                :session-name 'cli-session
                                :nickname nickname
                                :host host
                                :port (parse-integer port)
                                :channels (list channel))))
    (worker session)))

(defun launch (args)
  (let ((true-args (rest args)))
    (if (= (length true-args) 4)
        (apply #'launch-inner true-args)
        (format t "nickname host port channel~%"))))
