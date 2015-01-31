(in-package :btcl-net)

;;; constants
(defconstant +TESTNET3-MAGIC+ #x0709110b) ; network byte order
(defconstant +MAINNET-MAGIC+ #xfeb4bef9) ; network byte order

;;; connection structure
(defclass peer-connection ()
  ((host :initarg :host)
   (port :initarg :port)
   (net :initarg net :initform 'testnet3)
   (handshaken :initform #b00) ; low bit means version rec'd, 2nd is verack rec'd
   (tcp-socket)
   (write-stream)
   (read-stream :initform nil)
   (read-buffer :initform nil)))

;;; msg header definition and macro to create all the types
(bindata:define-tagged-binary-class p2p-msg ()
  ((magic u32le)
   (command (iso-8859-1-string :length 12))
   (len u32le)
   (checksum u32le))
  (:dispatch (find-msg-class command)))

(defun find-msg-class (name)
  (multiple-value-bind (sym status)
      (find-symbol (concatenate 'string "MSG-" (string-upcase name)))
    (declare (ignore status))
    sym))

(defgeneric checksum-payload (msg))

(defmacro define-p2p-msg (name slots)
  (macut:with-gensyms (objectvar streamvar slotvar bytesvar slottypevar)
    `(progn
       (bindata:define-binary-class ,name (p2p-msg) ,slots)
       (defmethod checksum-payload ((,objectvar ,name))
         (let ((,streamvar (ironclad:make-octet-output-stream)))
           (dolist (,slotvar ',slots)
             (let ((,slottypevar (cadr ,slotvar)))
              (bindata:write-value (if (typep ,slottypevar 'list) (car ,slottypevar) ,slottypevar) ,streamvar (slot-value ,objectvar (car ,slotvar)))))
           (let ((,bytesvar (ironclad:get-output-stream-octets ,streamvar)))
             (values (dsha256-checksum ,bytesvar)
                     (length ,bytesvar))))))))

;;; now come the message types in the p2p protocol
(define-p2p-msg msg-version
    ((version u32le)
     (services u64le)
     (timestamp u64le)
     (addr-recv version-net-addr)
     (addr-from version-net-addr)
     (nonce u64le)
     (user-agent varstr)
     (start-height u32le)
     (relay u8)))
;; helper
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
                                                  :ip-addr (build-ip-addr "0.0.0.0")
                                                  :port 0)
                        :nonce (random (expt 2 64))
                        :user-agent (make-varstr "/btcl:0.0.2/")
                        :start-height 0
                        :relay 1)))
    (multiple-value-bind (cksm len) (checksum-payload msg)
      (setf (slot-value msg 'checksum) cksm)
      (setf (slot-value msg 'len) len)
      (bindata:write-value 'msg-version (slot-value remote 'write-stream) msg))))

(define-p2p-msg msg-verack ())
;; another helper TODO: generate these
(defun send-verack (remote)
  (let ((msg (make-instance 'msg-verack
                            :magic +TESTNET3-MAGIC+
                            :command "verack")))
    (multiple-value-bind (cksm len) (checksum-payload msg)
      (setf (slot-value msg 'checksum) cksm)
      (setf (slot-value msg 'len) len)
      (bindata:write-value 'msg-verack (slot-value remote 'write-stream) msg))))

(define-p2p-msg msg-inv
    ((cnt varint)
     (inv-vectors (inv-vector-list :count cnt))))

(define-p2p-msg msg-getdata
    ((cnt varint)
     (inv-vectors (inv-vector-list :count cnt))))

(define-p2p-msg msg-addr
    ((cnt varint)
     (addresses (addr-list :count cnt))))


