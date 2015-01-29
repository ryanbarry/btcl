(in-package :btcl-net)

(defparameter *debug* t)

(defclass peer-connection ()
  ((host :initarg :host)
   (port :initarg :port)
   (net :initarg net :initform 'testnet3)
   (handshaken :initform #b00) ; low bit means version rec'd, 2nd is verack rec'd
   (tcp-socket)))

(defun event-cb (ev)
  (handler-case
      (error ev)
    (as:tcp-timeout ()
      (as:close-socket (as:tcp-socket ev)))
    (as:tcp-error ()
      (as:delay (lambda () (as:close-socket (as:tcp-socket ev)))))
    (as:tcp-eof ()
      (as:delay (lambda () (as:close-socket (as:tcp-socket ev)))))
    (error ()
      (when *debug*
        (format t "ev: ~a (~a)~%" (type-of ev) ev)))))

(defun make-read-cb (remote)
  (lambda (socket stream)
    (declare (ignore socket))
    (let ((msg (bindata:read-value 'p2p-msg stream)))
      (with-slots (command handshaken) msg
        (cond ((string= command "verack")
               (format t "got a verack!")
               (setf handshaken (boole boole-ior handshaken #b10)))
              ((string= command "version")
               (format t "got a version!")
               (setf handshaken (boole boole-ior handshaken #b01))
               (send-verack remote)))))))

(defun send-version (remote)
  (let ((msg (make-instance 'msg-version
                        :magic +TESTNET3-MAGIC+
                        :command "version"
                        :version 70002
                        :services 1
                        :timestamp (get-unix-time)
                        :addr-recv (make-instance 'version-net-addr
                                                  :services 1
                                                  :ip-addr (build-ip-addr (slot-value remote 'host))
                                                  :port (slot-value remote 'port))
                        :addr-from (make-instance 'version-net-addr
                                                  :services 1
                                                  :ip-addr (build-ip-addr '(10 0 1 185))
                                                  :port 0)
                        :nonce (random (expt 2 64))
                        :user-agent (make-varstr "/btcl:0.0.2/")
                        :start-height 0
                        :relay 1)))
    (multiple-value-bind (cksm len) (checksum-payload msg)
      (setf (slot-value msg 'checksum) cksm)
      (setf (slot-value msg 'len) len)
      (bindata:write-value 'msg-version (slot-value remote 'tcp-socket) msg))))

(defun send-verack (remote)
  (let ((msg (make-instance 'msg-verack
                            :magic +TESTNET3-MAGIC+
                            :command "verack")))
    (multiple-value-bind (cksm len) (checksum-payload msg)
      (setf (slot-value msg 'checksum) cksm)
      (setf (slot-value msg 'len) len)
      (bindata:write-value 'msg-version (slot-value remote 'tcp-socket) msg))))

(defun start-peer (remote)
  (as:with-event-loop (:catch-app-errors t)
    (as:signal-handler as:+sigint+
                     (lambda (sig)
                       (declare (ignore sig))
                       (format t "Closing peer...~%")
                       (as:exit-event-loop)))
    (setf (slot-value remote 'tcp-socket)
          (as:tcp-connect (slot-value remote 'host)
                          (slot-value remote 'port)
                          (make-read-cb remote)
                          :event-cb #'event-cb
                          :stream t))
    (send-version remote)))
