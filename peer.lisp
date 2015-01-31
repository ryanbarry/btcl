(in-package :btcl-net)

(defparameter *debug* t)

(defun event-cb (ev)
  (handler-case
      (error ev)
    (as:tcp-timeout ()
      (as:close-socket (as:tcp-socket ev)))
    (as:tcp-error ()
      (format t "tcp-error received, closing socket...")
      (as:delay (lambda () (as:close-socket (as:tcp-socket ev)))))
    (as:tcp-eof ()
      (format t "tcp-eof received (peer likely disconnected) closing socket...")
      (as:delay (lambda () (as:close-socket (as:tcp-socket ev)))))
    (error ()
      (when *debug*
        (format t "error-ev: ~a (~a)~%" (type-of ev) ev)))))

(define-condition invalid-msg () (reason))

(defun make-read-cb (remote)
  (lambda (socket bytevec)
    (declare (ignore socket))
    (with-slots (read-stream read-buffer) remote
      ;; put incoming data on input stream
      (setf read-stream (if (eql nil read-stream)
                            (ironclad:make-octet-input-stream bytevec)
                            (make-concatenated-stream read-stream (ironclad:make-octet-input-stream bytevec))))
      ;; try parsing message out of data collected so far
      (format t "~&trying to read msg...")
      (handler-case (message-handler remote)
        ;; if it can't read an entire message before eof'ing, put data back on stream
        (end-of-file ()
          (format t "eof'd before getting a whole message~%")
          (setf read-stream (if (eql nil read-stream)
                                (ironclad:make-octet-input-stream bytevec)
                                (make-concatenated-stream read-stream (ironclad:make-octet-input-stream bytevec)))))
        (invalid-msg (reason)
          (format t "~&error: bad msg! (~s)~%" reason))
        ;; if it's successful, clear buffer and continue
        (:no-error (handler-result)
          (declare (ignore handler-result)))))))

(defun message-handler (remote)
  (let ((msg (bindata:read-value 'p2p-msg (slot-value remote 'read-stream))))
    (with-slots (command checksum) msg
      ;; (let ((computed-cksm (checksum-payload msg)))
      ;;  (if (/= computed-cksm checksum)
      ;;      (progn
      ;;        (format t "~&checksum received: ~X~%checksum computed: ~X~%" checksum computed-cksm)
      ;;        (signal 'invalid-msg :bad-checksum))))
      (with-slots (handshaken) remote
       (cond ((string= command "verack")
              (format t "~&got a verack!")
              (setf handshaken (boole boole-ior handshaken #b10))
              (format t "~&handshaken: ~d~%" handshaken))
             ((string= command "version")
              (with-slots (version user-agent start-height) msg
                  (format t "~&receive version message: ~s: version ~d, blocks=~d" (slot-value user-agent 'str) version start-height))
              (setf handshaken (boole boole-ior handshaken #b01))
              (send-msg remote (prep-msg (make-instance 'verack)))
              (format t "~&handshaken: ~d~%" handshaken))
             ((string= command "inv")
              (format t "got some inventory!")
              (with-slots (magic command len checksum cnt inv-vectors) msg
                (format t "~&magic: ~X~%command: ~s~%len: ~d~%checksum: ~X" magic command len checksum)
                (format t "~&~tcount: ~d~%~tobj_type: ~d~%~thash: ~X~%"
                        cnt
                        (slot-value (car inv-vectors) 'obj-type)
                        (slot-value (car inv-vectors) 'hash))))
             (t (format t "~&got a new msg: ~s~%" command)))))))

(defun start-peer (remote)
  (as:with-event-loop (:catch-app-errors nil)
    (as:signal-handler as:+sigint+
                     (lambda (sig)
                       (declare (ignore sig))
                       (format t "~&got sigint, stopping peer...~%")
                       (as:exit-event-loop)))
    (let ((tcpsock (as:tcp-connect (slot-value remote 'host)
                                  (slot-value remote 'port)
                                  (make-read-cb remote)
                                  :event-cb #'event-cb)))
      (setf (slot-value remote 'tcp-socket) tcpsock)
      (setf (slot-value remote 'write-stream)
            (make-instance 'as:async-output-stream :socket tcpsock)))
    (send-msg remote (prep-msg (make-instance 'version
                                     :version 70002
                                     :services 1
                                     :timestamp (get-unix-time)
                                     :addr-recv (make-instance 'version-net-addr
                                                               :services 1
                                                               :ip-addr (build-ip-addr (slot-value remote 'host))
                                                               :port (slot-value remote 'port))
                                     :addr-from (make-instance 'version-net-addr
                                                               :services 1
                                                               :ip-addr (build-ip-addr "0.0.0.0")
                                                               :port 0)
                                     :nonce (random (expt 2 64))
                                     :user-agent (make-varstr "/btcl:0.0.2/")
                                     :start-height 0
                                     :relay 1)))))
