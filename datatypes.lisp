(in-package :btcl-net)

(bindata:define-binary-type raw-bytes (size)
  (:reader (in)
           (let ((buf (make-array size :element-type '(unsigned-byte 8))))
             (read-sequence buf in)
             buf))
  (:writer (out buf)
           (write-sequence buf out)))

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

(bindata:define-binary-class varstr ()
  ((len varint)
   (str (iso-8859-1-string :length len))))

(bindata:define-binary-type addr-list (count)
  (:reader (in)
           (loop for i from 0 upto count
              for addr = (bindata:read-value 'net-addr in)
              while addr
              collect addr))
  (:writer (out net-addrs)
           (dolist (net-addr net-addrs)
             (bindata:write-value 'net-addr out net-addr))))

(defun make-varstr (str)
  (make-instance 'varstr
                 :len (length str)
                 :str str))

(defgeneric build-ip-addr (addr)
  (:documentation "make a 16-byte address in network byte order"))

;; (defmethod build-ip-addr ((addr string))
;;   (let ((result 0))
;;     (setf (ldb (byte 8 80) result) 255) ;;;; 0xFFFF to represent IPv4
;;     (setf (ldb (byte 8 88) result) 255) ;;;; address within IPv6 address
;;     (loop with lowbit = 96
;;        with byte = 0
;;        for char across addr
;;        do (if (char= char #\.)
;;               (progn (setf (ldb (byte 8 lowbit) result) byte)
;;                      (incf lowbit 8)
;;                      (setf byte 0))
;;               (setf byte (+ (- (char-int char) 48) (* 10 byte))))
;;        finally (setf (ldb (byte 8 lowbit) result) byte))
;;     result))

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

(defmethod build-ip-addr (addr)
  (let ((ip-addr (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (elt ip-addr 10) 255)
    (setf (elt ip-addr 11) 255)
    (loop for byte in addr
         for i from 12 upto 16
       do (setf (elt ip-addr i) byte))
    ip-addr))

;; (defmethod build-ip-addr (addr)
;;   (if (or (< (length addr) 4) (some #'> addr '(255 255 255 255)))
;;       (error "You must specify the address as a list of 4 numbers, e.g. 192 168 1 10")
;;       (let ((result 0))
;;         (setf (ldb (byte 8 80) result) 255) ;;;; 0xFFFF to represent IPv4
;;         (setf (ldb (byte 8 88) result) 255) ;;;; address within IPv6 address
;;         (setf (ldb (byte 8 96) result) (elt addr 0))
;;         (setf (ldb (byte 8 104) result) (elt addr 1))
;;         (setf (ldb (byte 8 112) result) (elt addr 2))
;;         (setf (ldb (byte 8 120) result) (elt addr 3))
;;         result)))
