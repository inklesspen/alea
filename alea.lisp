;;;; alea.lisp

(in-package #:alea)

;;; "alea" goes here. Hacks and glory await!

;;; hooks: listen for commands

;;; sub-thread: define hooks, then read-message-loop on the connection

(defun worker (connection)
  (format t "Gonna do this action")
  (cl-irc:read-message-loop connection))

;;; main thread: make connection, hand off to subthread to process

(defvar *connections* (make-hash-table :test 'equal))

(defun connect (name host port nick channel)
  (let ((connection (cl-irc:connect :nickname nick
                                    :server host
                                    :port port)))
    (cl-irc:join connection channel)
    (setf (gethash name *connections*) connection)
    (flet ((do-loop ()
             (worker connection)))
      (sb-thread:make-thread #'do-loop)
      connection)))

