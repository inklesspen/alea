;;;; alea.lisp

(in-package #:alea)

;;; "alea" goes here. Hacks and glory await!

;;; hooks: listen for commands

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
   (sigil
    :documentation "The command character such as !"
    :initform #\!
    :initarg :sigil
    :reader sigil)
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

;; TODO some nickserv integration. it seems to work mainly with NOTICE from NickServ.


(defmethod handle-clirc-message ((session session) (message cl-irc:irc-rpl_welcome-message))
  (format t "~A~%" (describe (connection session)))
  (format t "~A~%" (describe message))
  (cl-irc:join (connection session) (channel session)))

(defun strict-commandp (message session)
  "A command is 'strict' if it begins with the bot's name or is in a private message."
  (let* ((bot-nickname (cl-irc:nickname (cl-irc:user (connection session))))
         (arguments (cl-irc:arguments message))
         (recipient (first arguments))
         (potential-command (second arguments)))
    (or (alexandria:starts-with-subseq bot-nickname potential-command :test #'string-equal)
        (string-equal bot-nickname recipient))))

(defun commandp (message session)
  (let* ((sigil (sigil session))
         (potential-command (second (cl-irc:arguments message)))
         (first-char (char potential-command 0)))
    (or (eql sigil first-char) (strict-commandp message session))))

(defmethod handle-clirc-message ((session session) (message cl-irc:irc-privmsg-message))
  (let* ((connection (connection session))
         (nickname (cl-irc:nickname (cl-irc:user connection)))
         (arguments (cl-irc:arguments message))
         (is-privmsg (string-equal (first arguments) nickname))
         (destination (if is-privmsg
                          (cl-irc:source message)
                          (first arguments)))
         (is-command (commandp message session)))
    (when is-command
      ;; need to parse out commands!
      (let* ((response (format nil "You said: ~a" (second arguments)))
             (response (if is-privmsg
                           response
                           (format nil "~a: ~a" (cl-irc:source message) response))))
        (cl-irc:privmsg connection destination response)))
    t))

(defun gather-handler-types ()
  "generic-function-methods gets the method metaobjects for the generic function. then method-specializers returns the two class specializers for the method; the second one is the class metaobject for the message, so calling class-name on it gets the symbol we need"
  (let ((methods (sb-mop:generic-function-methods #'handle-clirc-message))
        (extractor (lambda (method)
                     (class-name (second (sb-mop:method-specializers method))))))
    (mapcar extractor methods)))

(defun setup-hooks (session)
  (let ((connection (connection session))
        (hook-types (gather-handler-types))
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
