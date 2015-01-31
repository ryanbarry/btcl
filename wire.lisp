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
      (find-symbol (string-upcase name))
    (declare (ignore status))
    sym))

(defgeneric checksum-payload (msg))

(defgeneric prep-msg (msg))

(defgeneric send-msg (remote msg))

(defmacro define-p2p-msg (name slots)
  (macut:with-gensyms (objectvar streamvar slotvar bytesvar slottypevar remvar retcksmvar retlenvar)
    `(progn
       (bindata:define-binary-class ,name (p2p-msg) ,slots)
       (defmethod checksum-payload ((,objectvar ,name))
         (let ((,streamvar (ironclad:make-octet-output-stream)))
           (dolist (,slotvar ',slots)
             (let ((,slottypevar (cadr ,slotvar)))
              (bindata:write-value (if (typep ,slottypevar 'list) (car ,slottypevar) ,slottypevar) ,streamvar (slot-value ,objectvar (car ,slotvar)))))
           (let ((,bytesvar (ironclad:get-output-stream-octets ,streamvar)))
             (values (dsha256-checksum ,bytesvar)
                     (length ,bytesvar)))))
       (defmethod prep-msg ((,objectvar ,name))
         (with-slots (command checksum len) ,objectvar
           (multiple-value-bind (,retcksmvar ,retlenvar) (checksum-payload ,objectvar)
             (setf command (string-downcase (symbol-name ',name)))
             (setf checksum ,retcksmvar)
             (setf len ,retlenvar)
             ,objectvar)))
       (defmethod send-msg (,remvar (,objectvar ,name))
         (setf (slot-value ,objectvar 'magic) (symbol-value (find-symbol (concatenate 'string "+" (symbol-name (slot-value ,remvar 'net)) "-MAGIC+"))))
         (bindata:write-value ',name (slot-value ,remvar 'write-stream) ,objectvar)))))

;;; now come the message types in the p2p protocol
(define-p2p-msg version
    ((version u32le)
     (services u64le)
     (timestamp u64le)
     (addr-recv version-net-addr)
     (addr-from version-net-addr)
     (nonce u64le)
     (user-agent varstr)
     (start-height u32le)
     (relay u8)))

(define-p2p-msg verack ())

(define-p2p-msg inv
    ((cnt varint)
     (inv-vectors (inv-vector-list :count cnt))))

(define-p2p-msg getdata
    ((cnt varint)
     (inv-vectors (inv-vector-list :count cnt))))

(bindata:define-binary-class txn ()
    ((version u32le)
     (tx-in-count varint)
     (tx-in (tx-in-list :count tx-in-count))
     (tx-out-count varint)
     (tx-out (tx-out-list :count tx-out-count))
     (lock-time u32le)))

(define-p2p-msg tx
  ((version u32le)
   (tx-in-count varint)
   (tx-in (tx-in-list :count tx-in-count))
   (tx-out-count varint)
   (tx-out (tx-out-list :count tx-out-count))
   (lock-time u32le)))

(define-p2p-msg block
    ((version u32le)
     (hash-prev-block (raw-bytes :size 32))
     (hash-merkle-root (raw-bytes :size 32))
     (timestamp u32le)
     (bits u32le)
     (nonce u32le)
     (txn-count varint)
     (txn-list (txn-list :count txn-count))))

(define-p2p-msg addr
    ((cnt varint)
     (addresses (addr-list :count cnt))))
