(in-package :btcl-net)

;; constants
(defconstant +TESTNET3-MAGIC+ #x0709110b) ; network byte order
(defconstant +MAINNET-MAGIC+ #xfeb4bef9) ; network byte order

;; common structures
(bindata:define-binary-class version-net-addr ()
  ((services u64le)
   (ip-addr (raw-bytes :size 16))
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

(bindata:define-binary-class msg-version (p2p-msg)
  ((version s32le)
   (services u64le)
   (timestamp s64le)
   (addr-recv version-net-addr)
   (addr-from version-net-addr)
   (nonce u64le)
   (user-agent varstr)
   (start-height s32le)
   (relay u8)))

(bindata:define-binary-class msg-verack (p2p-msg) ())

(bindata:define-binary-class msg-addr (p2p-msg)
  ((#:count varint)
   (addresses addr-list)))

(defun find-msg-class (name)
  (multiple-value-bind (sym status)
      (find-symbol (concatenate 'string "MSG-" (string-upcase name)))
    (declare (ignore status))
    sym))

(defun write-value-to-vector (value)
  (let* ((vecstream (ironclad:make-octet-output-stream)))
    (bindata:write-value 'msg-version vecstream message)
    (ironclad:get-output-stream-octets vecstream)))

(defun frame-message (message)
  (let ((msg-bytes (write-value-to-vector message)))
    (msg-header (make-instance 'msg
                               :magic +TESTNET3-MAGIC+
                               :command "version"
                               :len (length msg-bytes)
                               :checksum (dsha256-checksum msg-bytes)))))


