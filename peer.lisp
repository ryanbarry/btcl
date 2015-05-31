(in-package :btcl-wire)

;;; TODO: all commented out (format)s need to be wrapped in a shim that allows
;;; them to be easily enabled/disabled

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

(defun accumulate-input (remote input-bytevec)
  (push input-bytevec (slot-value remote 'read-buffers)))

(defun sum-list-sequence-lengths (buffers)
  (reduce #'+ (map 'list #'length buffers) :initial-value 0))

(defun make-octet-input-stream (octet-vectors)
  (apply #'make-concatenated-stream
         (loop
            for ov in octet-vectors
            when (and (not (eql nil ov)) (> (length ov) 1))
            collect (ironclad:make-octet-input-stream ov))))

(defun octet-input-stream-to-vector (stream)
  (let ((s (ironclad:make-octet-output-stream)))
    (handler-case (loop for b = (read-byte stream nil 'eof)
                     until (eql b 'eof)
                     do (write-byte b s)))
    (ironclad:get-output-stream-octets s)))

;; in: list of byte vectors
;; out: (message or nil) and (list of byte vectors for remaining input)
(defun try-read-message (buffers)
  "given a list of octet vectors as a read buffer, try to read a message"
  ;; (format t "~&trying to read a message (buffer has ~d bytes)"
  ;;         (sum-list-sequence-lengths buffers))
  (if (>= (sum-list-sequence-lengths buffers)
          btcl-constants:+P2P-MSG-HEADER-LEN+)
      (let* ((instream (make-octet-input-stream buffers))
             (header (bindata:read-value 'p2p-msg-header instream)))
        (if (>= (sum-list-sequence-lengths buffers) (+ (slot-value header 'len)
                                                       btcl-constants:+P2P-MSG-HEADER-LEN+))
            (let* ((instream (make-octet-input-stream buffers))
                   (msg (bindata:read-value 'p2p-msg instream))
                   (leftovers (list (octet-input-stream-to-vector instream)))
                   ;; (leftovers-len (sum-list-sequence-lengths leftovers))
                   )
              ;; (format t "~&got a msg, ~d bytes left in buffer after reading" leftovers-len)
              ;; (if (> leftovers-len 0)
              ;;     (format t ", contents:~%~a" leftovers))
              (values msg leftovers))
            (progn
              ;; (format t "~%buffers only contain partial message (header-specified len: ~d, buffer len: ~d)"
              ;;         (slot-value header 'len) (sum-list-sequence-lengths buffers))
              ;; (if (> (slot-value header 'len) 33554432) ; constant from bitcoind's serialize.h
              ;;     (progn
              ;;       (format t "this message is greater than MAX_SIZE!~%~a" buffers)
              ;;       (-1)))
              (values nil buffers))))
      (progn
        ;; (format t "~%didn't get enough bytes for a header~%~%")
        (values nil buffers))))

(defun make-read-cb (remote)
  (lambda (socket bytevec)
    (declare (ignore socket))
    (accumulate-input remote bytevec)
    (with-slots (read-buffers) remote
     (loop with got-a-msg = t
        do (multiple-value-bind (msg buf-list)
               (try-read-message (reverse read-buffers))
             (if msg
                 (handle-message remote msg)
                 (setf got-a-msg nil))
             (setf read-buffers (reverse buf-list)))
        while (and got-a-msg
                   (>= (sum-list-sequence-lengths read-buffers)
                       btcl-constants:+P2P-MSG-HEADER-LEN+))))))

(defun handle-message (remote message)
  (with-slots (command checksum) (slot-value message 'header)
    (multiple-value-bind (computed-cksm length msg-hash) (checksum-payload message)
      (declare (ignore length))
      (if (/= computed-cksm checksum)
          (progn
            (format t "~&checksum received: ~X~%checksum computed: ~X~%" checksum computed-cksm)
            (signal 'invalid-msg :bad-checksum))
          ;(format t "checksum checks out!")
          )
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
               (format t "~&got an inv notification!")
               (with-slots (magic command len checksum cnt inv-vectors) (slot-value message 'header)
                 (with-slots (cnt inv-vectors) message
                  ;(format t "~&magic: ~X~%command: ~s~%len: ~d~%checksum: ~X" magic command len checksum)
                  (let ((interesting-inventory (loop for inv in inv-vectors
                                                  for objtype = (slot-value inv 'bty::obj-type)
                                                  for hsh = (slot-value inv 'bty::hash)
                                                  ;; do (format t "~&~tcount: ~d~%~tobj_type: ~d~%~thash: ~X~%"
                                                  ;;            cnt objtype hsh)
                                                  when (or (= objtype 1) (= objtype 2))
                                                  collect inv)))
                    (send-msg remote (prep-msg (make-instance 'msg-getdata
                                                              :cnt (length interesting-inventory)
                                                              :inv-vectors interesting-inventory)))))))
              ((string= command "tx")
               (format t "~&got a tx! ")
               ;; (with-slots (bty::version bty::lock-time) (slot-value message 'tx)
               ;;  (format t "version: ~a~%nLockTime: ~a~%" bty::version bty::lock-time))
               (btcl-web:notify-tx (slot-value message 'tx) msg-hash))
              ((string= command "block")
               (format t "~&got a block!~%")
               (btcl-web:notify-blk (slot-value message 'blk)))
              (t (format t "~&received message of unknown type: ~s~%" command)))))))

(defun start-peer (remote)
  (btcl-db:start)
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
