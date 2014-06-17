;;;; alea.lisp

(in-package #:alea)

;;; "alea" goes here. Hacks and glory await!


;;; sub-thread: define hooks, then read-message-loop on the connection
(defclass response ()
  ((session
    :initarg :session
    :initform (error "Must supply session")
    :reader session)
   (destination
    :initarg :destination
    :initform (error "Must supply destination")
    :reader destination)
   (addressee
    :initarg :addressee
    :initform nil
    :reader addressee)
   (in-response-to
    :initarg :in-response-to
    :initform (error "Must supply message this is in response to")
    :reader in-response-to)
   (text
    :initform nil
    :accessor text)))

(defgeneric send-response (response))
(defmethod send-response ((response response))
  ;; assemble full privmsg from text and addressee if any
  (with-accessors ((session session)
                   (destination destination)
                   (addressee addressee)
                   (text text)) response
    (unless text (error "Must set response text first"))
    (let ((connection (connection session))
          (full-text (if addressee
                         (format nil "~a: ~a" addressee text)
                         text)))
      ;; then send it
      (cl-irc:privmsg connection destination full-text))))

(defclass channel ()
  ((channel
    :initarg :channel
    :initform (error "Must supply channel")
    :reader channel)
   (context
    :initarg :context
    :initform nil
    :accessor context)))

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
   (channels
    :initarg :channels
    :initform ()
    :accessor channels)
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
  (dolist (channel (channels session))
    (cl-irc:join (connection session) (channel channel))))

(defun bot-nickname (session)
  (cl-irc:nickname (cl-irc:user (connection session))))

(defun message-recipient (message)
  (first (cl-irc:arguments message)))

(defun strict-commandp (message session)
  "A command is 'strict' if it begins with the bot's name or is in a private message."
  (let* ((bot-nickname (bot-nickname session))
         (recipient (message-recipient message))
         (potential-command (second (cl-irc:arguments message))))
    (or (alexandria:starts-with-subseq bot-nickname potential-command :test #'string-equal)
        (string-equal bot-nickname recipient))))

; replace this with (identify-command) which returns only the command, along with flags

(defun commandp (message session)
  (let* ((sigil (sigil session))
         (potential-command (second (cl-irc:arguments message)))
         (first-char (char potential-command 0)))
    (or (eql sigil first-char) (strict-commandp message session))))

(defun remove-first-word (string)
  (let* ((first-space (position #\space string))
         (next-nonspace (position #\space string :start (+ 1 first-space) :test (complement #'eql))))
    (subseq string next-nonspace)))

(defun identify-command (message session)
  (let* ((sigil (sigil session))
         (potential-command (second (cl-irc:arguments message)))
         (first-char (char potential-command 0))
         (bot-nickname (bot-nickname session)))
    (cond
      ((string-equal (bot-nickname session) (message-recipient message))
       ;; in a PM, the entire message is the command
       (list :strict t :command potential-command))
      ((alexandria:starts-with-subseq bot-nickname potential-command :test #'string-equal)
       ;; if the bot's nickname is there, take it from the first non-whitespace
       ;; after the first whitespace
       (list :strict t :command (remove-first-word potential-command)))
      ((eql sigil first-char)
       (let ((first-nonsigil (position sigil potential-command :test (complement #'eql))))
         (list :strict nil :command (subseq potential-command first-nonsigil))))
      (t nil))))

(defun pick-context (session destination is-privmsg)
  (let ((standard-context (make-instance 'context)))
    (if is-privmsg standard-context
        (alexandria:if-let (matching-channels (remove-if-not #'(lambda (chanobj) (string-equal (channel chanobj) destination)) (channels session)))
          (context (first matching-channels))
          standard-context))))

(defmethod handle-clirc-message ((session session) (message cl-irc:irc-privmsg-message))
  (let* ((connection (connection session))
         (nickname (cl-irc:nickname (cl-irc:user connection)))
         (arguments (cl-irc:arguments message))
         (is-privmsg (string-equal (first arguments) nickname))
         (destination (if is-privmsg
                          (cl-irc:source message)
                          (first arguments)))
         (command (identify-command message session)))
    (when command
      (let ((response (make-instance 'response
                                     :session session
                                     :in-response-to message
                                     :destination destination
                                     :addressee (unless is-privmsg (cl-irc:source message))))
            (context (pick-context session destination is-privmsg)))
        (format t "~a~%" command)
        ; gotta strip out the command sigil (name or !)
        (let ((parsed (parse-command context (getf command :command))))
          ;; if parse-error, check if strict
          (if (eql (car parsed) :parse-error)
              (if (getf command :strict) (setf (text response) "error yo"))
              (let ((result (eval-command context parsed)))
                (format t "~a~%" result)
                (setf (text response) result)))
          (format t "~a~%" parsed)
          (if (text response)
              (send-response response)
              ;; if we don't want to handle it, return t
              ;; that way cl-irc doesn't whine
              t))))))

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
  (let* ((channel (make-instance 'channel :channel channel :context (make-instance 'ore-context)))
         (session (make-instance 'session :session-name name :nickname nick :host host :port port :channels (list channel)))
         (stdout *standard-output*)
         (threadname (format nil "irc-handler-~S" name)))
    (setf (gethash name *sessions*) session)
    (sb-thread:make-thread (lambda ()
                             (let ((*standard-output* stdout))
                               (worker session))) :name threadname)
    session))
