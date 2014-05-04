;;;; alea.lisp

(in-package #:alea)

;;; "alea" goes here. Hacks and glory await!

;;; hooks: listen for commands

(defun privmsg-hook (message)
  (format t "~A~%" (describe message))
  (format t "~A~%" (cl-irc:arguments message))
  (format t "~A~%" (cl-irc:source message))
  t)

;;; !roll something
;;; Alea: roll something
;;; ! syntax doesn't squawk on errors; nick-syntax does

;;; sub-thread: define hooks, then read-message-loop on the connection

(defclass session ()
  ((connection
    :initarg :connection
    :initform (error "Must supply a connection")
    :accessor connection
    :documentation "cl-irc connection")
   (channel
    :initarg :channel
    :initform (error "Must supply channel")
    :accessor channel
    :documentation "Channel to join")))

(defgeneric handle-clirc-message (session message))

(defmethod handle-clirc-message ((session session) (message cl-irc:irc-rpl_welcome-message))
  (cl-irc:join (connection session) (channel session)))

(defmethod handle-clirc-message ((session session) (message cl-irc:irc-privmsg-message))
  (format t "~A~%" (describe (connection session)))
  (format t "~A~%" (describe message))
  t)

(defun setup-hooks (session)
  (let ((connection (connection session))
        (handler (curry #'handle-clirc-message session)))
    (cl-irc:remove-hooks connection 'cl-irc:irc-privmsg-message)
    (cl-irc:add-hook connection 'cl-irc:irc-privmsg-message handler)
    (cl-irc:add-hook connection 'cl-irc:irc-rpl_welcome-message handler)))

(defun worker (host port nick channel)
  (let* ((connection (cl-irc:connect :nickname nick
                                     :server host
                                     :port port))
         (session (make-instance 'session
                                 :connection connection
                                 :channel channel)))
    (setup-hooks session)
    (cl-irc:read-message-loop connection)))

;;; main thread: make connection, hand off to subthread to process
;;; or nope, just create the thread and tell it to connect

;;; we need to wait to join until we see CL-IRC:IRC-RPL_WELCOME-MESSAGE

(defvar *connections* (make-hash-table :test 'equal))

(defun connect (name host port nick channel)
    (let ((stdout *standard-output*)
          (threadname (format nil "irc-handler-~S" name)))
      (sb-thread:make-thread (lambda ()
                               (let ((*standard-output* stdout))
                                 (worker host port nick channel))) :name threadname)))
