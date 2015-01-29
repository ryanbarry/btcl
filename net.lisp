(in-package :btcl-net)

;; constants
(defconstant +TESTNET3-MAGIC+ #x0709110b) ; network byte order
(defconstant +MAINNET-MAGIC+ #xfeb4bef9) ; network byte order

;; common structures
(bindata:define-binary-class version-net-addr ()
  ((services u64le)
   (ip-addr (u128le :size 16))
   (port u16be)))

(bindata:define-binary-class net-addr ()
  ((timestamp u32le)
   (services u64le)
   (ip-addr (raw-bytes :size 16))
   (port u16be)))

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
  (macut:with-gensyms (objectvar streamvar slotvar bytesvar)
    `(progn
       (bindata:define-binary-class ,name (p2p-msg) ,slots)
       (defmethod checksum-payload ((,objectvar ,name))
         (let ((,streamvar (ironclad:make-octet-output-stream)))
           (dolist (,slotvar ',slots)
             (bindata:write-value (cadr ,slotvar) ,streamvar (slot-value ,objectvar (car ,slotvar))))
           (let ((,bytesvar (ironclad:get-output-stream-octets ,streamvar)))
             (values (dsha256-checksum ,bytesvar)
                     (length ,bytesvar))))))))

(define-p2p-msg msg-version
  ((version s32le)
   (services u64le)
   (timestamp s64le)
   (addr-recv version-net-addr)
   (addr-from version-net-addr)
   (nonce u64le)
   (user-agent varstr)
   (start-height s32le)
   (relay u8)))

(define-p2p-msg msg-verack ())

(define-p2p-msg msg-addr
  ((#:count varint)
   (addresses addr-list)))
