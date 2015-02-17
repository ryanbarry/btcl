(in-package :btcl-types)

;; binary blobs
(bindata:define-binary-type raw-bytes (size)
  (:reader (in)
           (let ((buf (make-array size :element-type '(unsigned-byte 8))))
             (read-sequence buf in)
             buf))
  (:writer (out buf)
           (write-sequence buf out)))

;; various integer formats
(bindata:define-binary-type unsigned-integer-be (bytes)
  (:reader (in)
           (loop with value = 0
              for low-bit downfrom (* 8 (1- bytes)) to 0 by 8 do
                (setf (ldb (byte 8 low-bit) value) (read-byte in))
              finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* 8 (1- bytes)) to 0 by 8
              do (write-byte (ldb (byte 8 low-bit) value) out))))

(defun read-unsigned-le (in bytes)
  (loop with value = 0
     for low-bit upto (* 8 (1- bytes)) from 0 by 8 do
       (setf (ldb (byte 8 low-bit) value) (read-byte in))
     finally (return value)))
(defun write-unsigned-le (out value bytes)
  (loop for low-bit upto (* 8 (1- bytes)) from 0 by 8
     do (write-byte (ldb (byte 8 low-bit) value) out)))

(bindata:define-binary-type unsigned-integer-le (bytes)
  (:reader (in) (read-unsigned-le in bytes))
  (:writer (out value) (write-unsigned-le out value bytes)))

(bindata:define-binary-type signed-integer-le (bytes)
  (:reader (in)
           (let ((rawnum (read-unsigned-le in bytes))
                 (bytes-max (expt 2 bytes)))
             (if (>= rawnum (/ bytes-max 2))
                 (- bytes-max rawnum)
                 rawnum)))
  (:writer (out value)
           (if (>= value 0)
               (write-unsigned-le out value bytes)
               (error "Writing negative values isn't implemented yet lulz"))))

;; just one byte, can't technically be little- or big-endian, w/e
(bindata:define-binary-type u8 () (unsigned-integer-le :bytes 1))

;; big-endian types used
(bindata:define-binary-type u16be () (unsigned-integer-be :bytes 2))
(bindata:define-binary-type u32be () (unsigned-integer-be :bytes 4))
(bindata:define-binary-type u64be () (unsigned-integer-be :bytes 8))

;; little-endian
(bindata:define-binary-type u16le () (unsigned-integer-le :bytes 2))
(bindata:define-binary-type u32le () (unsigned-integer-le :bytes 4))
(bindata:define-binary-type u64le () (unsigned-integer-le :bytes 8))
(bindata:define-binary-type s32le () (signed-integer-le :bytes 4))
(bindata:define-binary-type s64le () (signed-integer-le :bytes 8))

;; CompactSize in BitcoinQT
(bindata:define-binary-type varint ()
  (:reader (in)
           (let ((lowbyte (read-byte in)))
             (cond ((< lowbyte #xfd) lowbyte)
                   ((= lowbyte #xfd) (read-unsigned-le in 2))
                   ((= lowbyte #xfe) (read-unsigned-le in 4))
                   ((= lowbyte #xff) (read-unsigned-le in 8)))))
  (:writer (out value)
           (cond ((< value #xfd) (write-unsigned-le out value 1))
                 ((<= value #xffff) (progn (write-byte #xfd out)
                                           (write-unsigned-le out value 2)))
                 ((<= value #xffffffff) (progn (write-byte #xfe out)
                                               (write-unsigned-le out value 4)))
                 (t (progn (write-byte #xff out)
                           (write-unsigned-le out value 8))))))

;; string of defined length, in characters, where char type is defined separately
(bindata:define-binary-type generic-string (length character-type)
  (:reader (in)
           (let ((string (make-string length))
                 (end 0))
             (loop for i from 0 below length
                for char = (bindata:read-value character-type in)
                do (setf (char string i) char)
                unless (char= char #\null) do (incf end) end
                finally (return (subseq string 0 end)))))
  (:writer (out string)
           (dotimes (i (length string))
             (bindata:write-value character-type out (char string i)))
           (if (> length (length string))
               (dotimes (i (- length (length string)))
                 (write-byte 0 out)))))

;; character type meant to be used with generic-string
(bindata:define-binary-type iso-8859-1-char ()
  (:reader (in)
           (let ((code (read-byte in)))
             (or (code-char code)
                 (error "Character code ~d not supported" code))))
  (:writer (out char)
           (let ((code (char-code char)))
             (if (<= 0 code #xff)
                 (write-byte code out)
                 (error "Illegal character for iso-8859-1 encoding: character: ~c with code: ~d" char code)))))

;; concrete string type
(bindata:define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

;; iso-8859-1 string whose length is defined with a varint
(bindata:define-binary-type varstr ()
  (:reader (in)
           (let ((len (bindata:read-value 'varint in)))
             (bindata:read-value 'iso-8859-1-string in :length len)))
  (:writer (out value)
           (let ((len (length value)))
             (bindata:write-value 'varint out len)
             (bindata:write-value 'iso-8859-1-string out value :length len))))

;; structure used to describe node addresses
(bindata:define-binary-class net-addr ()
  ((timestamp u32le)
   (services u64le)
   (ip-addr (raw-bytes :size 16))
   (port u16be)))

;; special form of net-addr used in the "version" message
(bindata:define-binary-class version-net-addr ()
  ((services u64le)
   (ip-addr (raw-bytes :size 16))
   (port u16be)))

;; list of net-addr
(bindata:define-binary-type addr-list (count)
  (:reader (in)
           (loop repeat count
              collect (bindata:read-value 'net-addr in)))
  (:writer (out net-addrs)
           (dolist (net-addr net-addrs)
             (bindata:write-value 'net-addr out net-addr))))

;; identifier for "inventory" (blocks, transactions, etc)
(bindata:define-binary-class inv-vector ()
  ((obj-type u32le)
   (hash (raw-bytes :size 32))))

;; list of inv-vector
(bindata:define-binary-type inv-vector-list (count)
  (:reader (in)
           (loop repeat count
              collect (bindata:read-value 'inv-vector in)))
  (:writer (out inv-vectors)
           (dolist (inv-vec inv-vectors)
             (bindata:write-value 'inv-vector out inv-vec))))

;; pointer to prior transaction's output
(bindata:define-binary-class outpoint ()
  ((hash (raw-bytes :size 32))
   (index u32le)))

;; transaction input
(bindata:define-binary-class tx-in ()
  ((previous-output outpoint)
   (script-len varint)
   (script-sig (raw-bytes :size script-len))
   (seq u32le)))

;; list of transaction inputs
(bindata:define-binary-type tx-in-list (count)
  (:reader (in)
           (loop repeat count
              collect (bindata:read-value 'tx-in in)))
  (:writer (out tx-in-list)
           (dolist (tx-in tx-in-list)
             (bindata:write-value 'tx-in out tx-in))))

;; transaction output
(bindata:define-binary-class tx-out ()
  ((value u64le)
   (script-len varint)
   (script-pk (raw-bytes :size script-len))))

;; list of transaction outputs
(bindata:define-binary-type tx-out-list (count)
  (:reader (in)
           (loop repeat count
              collect (bindata:read-value 'tx-out in)))
  (:writer (out tx-out-list)
           (dolist (tx-out tx-out-list)
             (bindata:write-value 'tx-out out tx-out))))

(bindata:define-binary-class tx ()
    ((version u32le)
     (tx-in-count varint)
     (tx-in (tx-in-list :count tx-in-count))
     (tx-out-count varint)
     (tx-out (tx-out-list :count tx-out-count))
     (lock-time u32le)))

;; list of transactions
(bindata:define-binary-type tx-list (count)
  (:reader (in)
           (loop repeat count
              collect (bindata:read-value 'tx in)))
  (:writer (out tx-list)
           (dolist (txn tx-list)
             (bindata:write-value 'tx out txn))))

;; block (named in order to not conflict with CL's block)
(bindata:define-binary-class blk ()
  ((version u32le)
   (hash-prev-block (raw-bytes :size 32))
   (hash-merkle-root (raw-bytes :size 32))
   (timestamp u32le)
   (bits u32le)
   (nonce u32le)
   (tx-count varint)
   (tx-list (tx-list :count tx-count))))

(defgeneric hash (obj))

(defmethod hash ((obj tx))
  (let ((octstream (ironclad:make-octet-output-stream)))
    (bindata:write-value 'tx octstream obj)
    (btcl-digest:dsha256 (ironclad:get-output-stream-octets octstream))))

(defmethod hash ((obj blk))
  (let ((octstream (ironclad:make-octet-output-stream)))
    (bindata:write-value 'blk octstream obj)
    (btcl-digest:dsha256 (ironclad:get-output-stream-octets octstream))))

;;; helpers

(defgeneric build-ip-addr (addr)
  (:documentation "make a 16-byte address in network byte order"))

;; make it from a dotted-quad format string, e.g. "192.168.1.25"
(defmethod build-ip-addr ((addr string))
  (let ((ip-addr (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (elt ip-addr 10) 255) ;;;; 0xFFFF to represent IPv4
    (setf (elt ip-addr 11) 255) ;;;; address within IPv6 address
    (loop for char across addr
       with pos = 12
       with byte = 0
       when (char= char #\.)
       do (progn
            (setf (elt ip-addr pos) byte)
            (incf pos)
            (setf byte 0))
       else do (setf byte (+ (- (char-int char) 48) (* 10 byte)))
       finally (setf (elt ip-addr pos) byte))
    ip-addr))

;; make it from a dotted-quad-like list, e.g. '(192 168 1 25)
(defmethod build-ip-addr ((addr list))
  (let ((ip-addr (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (elt ip-addr 10) 255)
    (setf (elt ip-addr 11) 255)
    (loop for byte in addr
         for i from 12 upto 16
       do (setf (elt ip-addr i) byte))
    ip-addr))
