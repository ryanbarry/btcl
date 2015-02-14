(in-package :btcl-wire)

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
    (accumulate-input remote bytevec)
    (if (> (sizeof-read-buffer remote) btcl-constants:+P2P-MSG-HEADER-LEN+)
        (let ((header (bindata:read-value
                       'p2p-msg-header
                       (get-octet-input-stream remote
                                               btconst:+P2P-MSG-HEADER-LEN+))))
          (let ((msg-len (+ (slot-value header 'len)
                            btconst:+P2P-MSG-HEADER-LEN+)))
            (if (>= (sizeof-read-buffer remote) msg-len)
                (let* ((instream (get-octet-input-stream remote))
                       (msg (bindata:read-value 'p2p-msg instream)))
                  (handler-case (handle-message remote msg)
                    (invalid-msg (reason)
                      (format t "~&error: bad msg! (~s)~%" reason)))
                  ;; if it's successful, clear buffer and continue
                  (setf (slot-value remote 'read-buffers)
                        (list (octet-input-stream-to-vector instream))))))))))

(defun handle-message (remote message)
  (with-slots (command checksum) message
    (multiple-value-bind (computed-cksm length msg-hash) (checksum-payload message)
      (declare (ignore length))
      (if (/= computed-cksm checksum)
          (progn
            (format t "~&checksum received: ~X~%checksum computed: ~X~%" checksum computed-cksm)
            (signal 'invalid-msg :bad-checksum))
          (format t "checksum checks out!"))
      (with-slots (handshaken) remote
        (cond ((string= command "verack")
               (format t "~&got a verack!")
               (setf handshaken (boole boole-ior handshaken #b10))
               (format t "~&handshaken: ~d~%" handshaken))
              ((string= command "version")
               (with-slots (version user-agent start-height) message
                 (format t "~&receive version message: ~s: version ~d, blocks=~d" user-agent version start-height))
               (setf handshaken (boole boole-ior handshaken #b01))
               (send-msg remote (prep-msg (make-instance 'msg-verack)))
               (format t "~&handshaken: ~d~%" handshaken))
              ((string= command "inv")
               (format t "~&got some inventory!")
               (with-slots (magic command len checksum cnt inv-vectors) message
                 (format t "~&magic: ~X~%command: ~s~%len: ~d~%checksum: ~X" magic command len checksum)
                 (let ((interesting-inventory (loop for inv in inv-vectors
                                                 for objtype = (slot-value inv 'bty::obj-type)
                                                 for hsh = (slot-value inv 'bty::hash)
                                                 do (format t "~&~tcount: ~d~%~tobj_type: ~d~%~thash: ~X~%"
                                                            cnt objtype hsh)
                                                 when (or (= objtype 1) (= objtype 2))
                                                 collect inv)))
                   (send-msg remote (prep-msg (make-instance 'msg-getdata
                                                             :cnt (length interesting-inventory)
                                                             :inv-vectors interesting-inventory))))))
              ((string= command "tx")
               (format t "~&got a tx!")
               (let ((uidata '()))
                 (with-slots (bty::tx-in-count bty::tx-out-count bty::tx-out) (slot-value message 'tx)
                   (let ((tx-total-value (/ (loop for txo in tx-out
                                               sum (slot-value txo 'bty::value)) 100000000)))
                     (setf uidata (acons "type" "tx" uidata))
                     (setf uidata (acons "hash" (format nil "~a" (ironclad:byte-array-to-hex-string (reverse msg-hash))) uidata))
                     (setf uidata (acons "tx-in-count" bty::tx-in-count uidata))
                     (setf uidata (acons "tx-out-count" bty::tx-out-count uidata))
                     (setf uidata (acons "total-sent" (format nil "~8,1,,$" tx-total-value) uidata)))
                   (btcl-web:publish! (cl-json:encode-json-alist-to-string uidata)))))
              ((string= command "block")
               (format t "~&got a block!~%")
               (let ((uidata '()))
                 (with-slots (bty::tx-count bty::timestamp bty::bits) (slot-value message 'blk)
                   (setf uidata (acons "type" "block" uidata))
                   (setf uidata (acons "numtx" bty::tx-count uidata))
                   (setf uidata (acons "timestamp" bty::timestamp uidata))
                   (setf uidata (acons "diff" (/ #xFFFF0000000000000000000000000000000000000000000000000000
                                                 (* (ldb (byte 24 0) bty::bits) (expt 2 (* 8 (- (ldb (byte 8 24) bty::bits) 3))))) uidata)))
                 (btcl-web:publish! (cl-json:encode-json-alist-to-string uidata))))
              (t (format t "~&got a new message: ~s~%" command)))))))

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
      (setf (slot-value remote 'write-stream) (make-instance 'as:async-output-stream :socket tcpsock)))
    (send-msg remote (prep-msg (make-instance 'msg-version
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
                                     :user-agent "/btcl:0.0.2/"
                                     :start-height 0
                                     :relay 1)))
    (btcl-web:start-server)))
