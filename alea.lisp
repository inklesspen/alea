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

(defclass place ()
  ((name
    :initarg :name
    :initform (error "Must supply name")
    :reader name)
   (is-public
    :initarg :is-public
    :initform t
    :reader is-public)
   (members
    :initarg :members
    :initform nil
    :accessor members)
   (context
    :initarg :context
    :initform (error "Must supply context")
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
   (init-channels
    :initarg :channels
    :initform nil
    :reader init-channels)
   (default-context
    :initarg :default-context
    :initform 'generic-context
    :reader default-context)
   (channels
    :initform nil
    :accessor channels)
   (privmsgs
    :initform nil
    :accessor privmsgs)
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

(defmethod handle-clirc-message ((session session) (message cl-irc:irc-join-message))
  (let ((nickname (cl-irc:source message))
        (channel (first (cl-irc:arguments message))))
    (format t "~a joined channel ~a~%" nickname channel)
    (when (string-equal (bot-nickname session) nickname)
      ;; bot just joined this channel
      (let* ((new-context (make-instance (default-context session)))
             ;; members will be filled in when it gets its first command
             (new-place (make-instance 'place :context new-context :is-public t :name channel)))
        (push (cons channel new-place) (channels session)))))
  ;; return nil so the default hook fires also
  nil)

(defmethod handle-clirc-message ((session session) (message cl-irc:irc-rpl_welcome-message))
  (format t "~A~%" (describe (connection session)))
  (format t "~A~%" (describe message))
  (dolist (channel (init-channels session))
    (cl-irc:join (connection session) channel)))

(defun bot-nickname (session)
  (cl-irc:nickname (cl-irc:user (connection session))))

(defun message-recipient (message)
  (first (cl-irc:arguments message)))

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

(defun identify-users (session channel)
  ;;; TODO: should exclude bot's nickname
  (alexandria:hash-table-keys (cl-irc:users (cl-irc:find-channel (connection session) channel))))

(defun identify-relevant-place (message session)
  (let* ((bot-nickname (bot-nickname session))
         (arguments (cl-irc:arguments message))
         (is-privmsg (string-equal (first arguments) bot-nickname))
         (destination (if is-privmsg
                          (cl-irc:source message)
                          (first arguments))))
    (when is-privmsg
      (let ((cell (assoc destination (privmsgs session) :test #'string-equal)))
        (unless cell
          ;;; make a privmsg place
          (let* ((new-context (make-instance (default-context session)))
                 (new-place (make-instance 'place :context new-context :is-public nil :name destination :members (list destination)))
                 (new-cons (cons destination new-place)))
            (push new-cons (privmsgs session))))))
    (cdr (assoc destination (slot-value session (if is-privmsg 'privmsgs 'channels)) :test #'string-equal))))


(defmethod handle-clirc-message ((session session) (message cl-irc:irc-privmsg-message))
  (let
      ;;; identify the relevant place (creating a privmsg place if needed)
      ((place (identify-relevant-place message session)))
    ;;; update the place's members if it's a public place
    (when (is-public place)
      (let ((members (identify-users session (name place))))
        (setf (members place) members)))
    ;;; identify a command if any
    (let ((command (identify-command message session))
          (context (context place)))
      (if command
          ;;; parse command. if parse-error and strict command, error message
          (let ((response (make-instance 'response
                                         :session session
                                         :in-response-to message
                                         :destination (name place)
                                         :addressee (when (is-public place) (cl-irc:source message))))
                (parsed (parse-command context (getf command :command))))
            (if (eql (car parsed) :parse-error)
                (if (getf command :strict) (setf (text response) "error yo"))
                ;;; eval command on context and place.
                (let ((result (eval-command context parsed)))
                  (setf (text response) result)))
            ;;; return the response if any
            (if (text response)
              (send-response response)
              ;; if we don't want to handle it, return t
              ;; that way cl-irc doesn't whine
              t))
          ;; we're doing nothing, just return t
          t))))


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
  (let* ((session (make-instance 'session :session-name name :nickname nick :host host :port port :channels (list channel)))
         (stdout *standard-output*)
         (threadname (format nil "irc-handler-~S" name)))
    (setf (gethash name *sessions*) session)
    (sb-thread:make-thread (lambda ()
                             (let ((*standard-output* stdout))
                               (worker session))) :name threadname)
    session))
