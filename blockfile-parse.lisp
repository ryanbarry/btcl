(in-package :com-nuclearice-btcl)

(defparameter *local-blockfile0* #p"/Users/rbarry/projects/btcl/blocks/blk0000000.dat")

(defun file->bytevec (pathname)
  (with-open-file (finstream pathname :element-type '(unsigned-byte 8))
    (let* ((filelen (file-length in))
           (filevec (make-array filelen :element-type '(unsigned-byte 8))))
      (read-sequence filevec finstream)
      filevec)))

(defun read-uint16 (bytevec)
  (let ((uint16 0))
    (setf (ldb (byte 8 0) uint16) (elt bytevec 0))
    (setf (ldb (byte 8 8) uint16) (elt bytevec 1))
    uint16))

(defun read-uint32 (bytevec)
  (let ((uint32 0))
    (setf (ldb (byte 8 0) uint32) (elt bytevec 0))
    (setf (ldb (byte 8 8) uint32) (elt bytevec 1))
    (setf (ldb (byte 8 16) uint32) (elt bytevec 2))
    (setf (ldb (byte 8 24) uint32) (elt bytevec 3))
    uint32))

(defun read-uint64 (bytevec)
  (let ((uint64 0))
    (setf (ldb (byte 8 0) varint) (elt bytevec 0))
    (setf (ldb (byte 8 8) uint64) (elt bytevec 1))
    (setf (ldb (byte 8 16) uint64) (elt bytevec 2))
    (setf (ldb (byte 8 24) uint64) (elt bytevec 3))
    (setf (ldb (byte 8 32) uint64) (elt bytevec 4))
    (setf (ldb (byte 8 40) uint64) (elt bytevec 5))
    (setf (ldb (byte 8 48) uint64) (elt bytevec 6))
    (setf (ldb (byte 8 56) uint64) (elt bytevec 7))
    uint64))

(defun read-varint (instream)
  (let ((varint 0))
    (setf (ldb (byte 8 0) varint) (read-byte instream))
    (cond ((< varint #xFD) varint)
	  ((= varint #xFD) ; 1st byte == 0xFD, then uint16_t
	   (read-uint16 instream))
	  ((= varint #xFE) ; 1st byte == 0xFE, then uint32_t
	   (read-uint32 instream))
	  ((= varint #xFF) ; 1st byte == 0xFF, then uint64_t
	   (read-uint64 instream))))
  varint)
