(in-package :btcl-wire)

;;; connection structure
(defclass peer-connection ()
  ((host :initarg :host)
   (port :initarg :port)
   (net :initarg net :initform 'testnet3)
   (handshaken :initform #b00) ; low bit means version rec'd, 2nd is verack rec'd
   (tcp-socket)
   (write-stream)
   (read-stream :initform (ironclad:make-octet-input-stream
                           (make-array 0 :element-type '(unsigned-byte 8))))
   (read-buffers :initform '())))

;;; msg header definition and macro to create all the types
(bindata:define-tagged-binary-class p2p-msg ()
  ((magic u32le)
   (command (iso-8859-1-string :length 12))
   (len u32le)
   (checksum u32le))
  (:dispatch (find-msg-class command)))

(defun find-msg-class (name)
  (multiple-value-bind (sym status)
      (find-symbol (concatenate 'string "MSG-" (string-upcase name)) :btcl-wire)
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
             (multiple-value-bind (checksum hash) (dsha256-checksum ,bytesvar)
                 (values checksum
                         (length ,bytesvar)
                         hash)))))
       (defmethod prep-msg ((,objectvar ,name))
         (with-slots (command checksum len) ,objectvar
           (multiple-value-bind (,retcksmvar ,retlenvar) (checksum-payload ,objectvar)
             (setf command (subseq (string-downcase (symbol-name ',name)) 4))
             (setf checksum ,retcksmvar)
             (setf len ,retlenvar)
             ,objectvar)))
       (defmethod send-msg (,remvar (,objectvar ,name))
         (setf (slot-value ,objectvar 'magic) (symbol-value (find-symbol (concatenate 'string "+" (symbol-name (slot-value ,remvar 'net)) "-MAGIC+") :btcl-constants)))
         (bindata:write-value ',name (slot-value ,remvar 'write-stream) ,objectvar)))))

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

(define-p2p-msg msg-verack ())

(define-p2p-msg msg-inv
    ((cnt varint)
     (inv-vectors (inv-vector-list :count cnt))))

(define-p2p-msg msg-getdata
    ((cnt varint)
     (inv-vectors (inv-vector-list :count cnt))))

(define-p2p-msg msg-tx
    ((tx tx)))

(define-p2p-msg msg-block
    ((blk blk)))

(define-p2p-msg msg-addr
    ((cnt varint)
     (addresses (addr-list :count cnt))))
