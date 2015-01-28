(in-package :btcl-net)

;; network byte order
(defconstant +TESTNET3-MAGIC+ #x0709110b)

(bindata:define-binary-type unsigned-integer-be (bytes)
  (:reader (in)
           (loop with value = 0
              for low-bit downfrom (* 8 (1- bytes)) to 0 by 8 do
                (setf (ldb (byte 8 low-bit) value) (read-byte in))
              finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* 8 (1- bytes)) to 0 by 8
              do (write-byte (ldb (byte 8 low-bit) value) out))))
(bindata:define-binary-type u16be () (unsigned-integer-be :bytes 2))
(bindata:define-binary-type u32be () (unsigned-integer-be :bytes 4))
(bindata:define-binary-type u64be () (unsigned-integer-be :bytes 8))

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
(bindata:define-binary-type u8 () (unsigned-integer-le :bytes 1))
(bindata:define-binary-type u16le () (unsigned-integer-le :bytes 2))
(bindata:define-binary-type u32le () (unsigned-integer-le :bytes 4))
(bindata:define-binary-type u64le () (unsigned-integer-le :bytes 8))
(bindata:define-binary-type u128le () (unsigned-integer-le :bytes 16))

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

(bindata:define-binary-type s32le () (signed-integer-le :bytes 4))
(bindata:define-binary-type s64le () (signed-integer-le :bytes 8))

;; this is CompactSize in BitcoinQT
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

(bindata:define-binary-type generic-string (length character-type)
  (:reader (in)
           (let ((string (make-string length)))
             (dotimes (i length)
               (setf (char string i) (bindata:read-value character-type in)))
             string))
  (:writer (out string)
           (dotimes (i (length string))
             (bindata:write-value character-type out (char string i)))
           (if (> length (length string))
               (dotimes (i (- length (length string)))
                 (write-byte 0 out)))))

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

(bindata:define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(bindata:define-binary-class version-net-addr ()
  ((services u64le)
   (ip-addr u128le)
   (port u16be)))

(bindata:define-binary-class header ()
  ((magic u32le)
   (command (iso-8859-1-string :length 12))
   (len u32le)
   (checksum u32le)))

(bindata:define-binary-class varstr ()
  ((len varint)
   (str (iso-8859-1-string :length len))))

(defun make-varstr (str)
  (make-instance 'varstr
                :len (length str)
                :str str))

(bindata:define-binary-class msg-version ()
  ((version s32le)
   (services u64le)
   (timestamp s64le)
   (addr-recv version-net-addr)
   (addr-from version-net-addr)
   (nonce u64le)
   (user-agent varstr)
   (start-height s32le)
   (relay u8)))

(defun build-ip-addr (&rest addr)
  "make a 16-byte address in network byte order"
  (if (or (< (length addr) 4) (some #'> addr '(255 255 255 255)))
      (error "You must specify the address as 4 numbers, e.g. 192 168 1 10")
      (let ((result 0))
        (setf (ldb (byte 8 80) result) 255) ;;;; 0xFFFF to represent IPv4
        (setf (ldb (byte 8 88) result) 255) ;;;; address within IPv6 address
        (setf (ldb (byte 8 96) result) (elt addr 0))
        (setf (ldb (byte 8 104) result) (elt addr 1))
        (setf (ldb (byte 8 112) result) (elt addr 2))
        (setf (ldb (byte 8 120) result) (elt addr 3))
        result)))
