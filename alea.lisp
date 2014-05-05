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
  ((session-name
    :initarg :session-name
    :initform (error "Must supply session-name")
    :reader session-name)
   (nickname
    :initarg :nickname
    :initform (error "Must supply nickname")
    :reader nickname)
   (host
    :initarg :host
    :initform (error "Must supply host")
    :reader host)
   (port
    :initarg :port
    :initform 6667
    :reader port)
   (channel
    :initarg :channel
    :initform (error "Must supply channel")
    :reader channel)
   (connection
    :initarg :connection
    :initform nil
    :accessor connection
    :documentation "cl-irc connection")
   (handler
    :initform nil)))

(defmethod initialize-instance :after ((session session) &key)
  (setf (slot-value session 'handler) (curry #'handle-clirc-message session)))

(defgeneric session-connect (session))
(defmethod session-connect ((session session))
  (with-accessors ((host host)
                   (port port)
                   (nickname nickname)
                   (connection connection)) session
    (setf connection (cl-irc:connect :nickname nickname :server host :port port))
    (setup-hooks session)
    session))

(defgeneric handle-clirc-message (session message))

(defmethod handle-clirc-message ((session session) (message cl-irc:irc-err_nicknameinuse-message))
  ;; Need to generate a new nickname and switch to that.
  t)


(defmethod handle-clirc-message ((session session) (message cl-irc:irc-rpl_welcome-message))
  (format t "~A~%" (describe (connection session)))
  (format t "~A~%" (describe message))
  (cl-irc:join (connection session) (channel session)))

(defmethod handle-clirc-message ((session session) (message cl-irc:irc-privmsg-message))
  (let* ((connection (connection session))
         (nickname (cl-irc:nickname (cl-irc:user connection)))
         (destination (if (string-equal (first (cl-irc:arguments message)) nickname)
                          (cl-irc:source message)
                          (first (cl-irc:arguments message)))))
    (cl-irc:privmsg connection destination (second (cl-irc:arguments message)))))

(defun setup-hooks (session)
  (let ((connection (connection session))
        (hook-types (mapcar (lambda (m) (class-name (second (sb-mop:method-specializers m)))) (sb-mop:generic-function-methods #'handle-clirc-message)))
        (handler (slot-value session 'handler)))
    (dolist (type hook-types)
      (cl-irc:remove-hook connection type handler)
      (cl-irc:add-hook connection type handler))))

(defun worker (session)
  (session-connect session)
  (cl-irc:read-message-loop (connection session)))

;;; main thread: make connection, hand off to subthread to process
;;; or nope, just create the thread and tell it to connect

;;; we need to wait to join until we see CL-IRC:IRC-RPL_WELCOME-MESSAGE

(defvar *sessions* (make-hash-table :test 'equal))

(defun connect (name host port nick channel)
  (let ((session (make-instance 'session :session-name name :nickname nick :host host :port port :channel channel))
        (stdout *standard-output*)
        (threadname (format nil "irc-handler-~S" name)))
    (setf (gethash name *sessions*) session)
    (sb-thread:make-thread (lambda ()
                             (let ((*standard-output* stdout))
                               (worker session))) :name threadname)
    session))
