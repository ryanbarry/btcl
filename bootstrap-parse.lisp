(in-package :com-nuclearice-btcl)

(defparameter *local-bootstrap* #p"/Volumes/downloads/incomplete/bootstrap.dat")
(defparameter *local-blk-storage* #p"/Users/rbarry/projects/btcl/blocks")

(defun parse-bootstrap-file (&key (bootstrap-file *local-bootstrap*)
			       (output-dir *local-blk-storage*)
			       (start 0)
			       (count 1))
  (with-open-file (in
		   bootstrap-file
		   :element-type '(unsigned-byte 8))
    (dotimes (blocknum (+ count start))
      (let ((network (read-uint32 in))
	    (blocksize (read-uint32 in)))
	(format t "block offset ~7d - net 0x~X size ~d"
		blocknum
		network blocksize)
	(if (< blocknum start)
	    (progn
	      (advance-file-position in blocksize)
	      (format t "~%"))
	    (with-open-file (out (make-blockfile-pathname blocknum output-dir)
			     :direction :output
			     :element-type '(unsigned-byte 8)
			     :if-does-not-exist :create
			     :if-exists :supersede)
	      (copy-bytes in out blocksize)
	      (format t " *~%")))))))

(defun file->bytevec (pathname)
  (with-open-file (finstream pathname :element-type '(unsigned-byte 8))
    (let* ((filelen (file-length in))
           (filevec (make-array filelen :element-type '(unsigned-byte 8))))
      (read-sequence filevec finstream)
      filevec)))

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

(defun read-uint16 (instream)
  (let ((uint16 0))
    (setf (ldb (byte 8 0) uint16) (read-byte instream))
    (setf (ldb (byte 8 8) uint16) (read-byte instream))
    uint16))

(defun make-uint16 (bytevec)
  (let ((uint16 0))
    (setf (ldb (byte 8 0) uint16) (elt bytevec 0))
    (setf (ldb (byte 8 8) uint16) (elt bytevec 1))
    uint16))

(defun read-uint32 (instream)
  (let ((uint32 0))
    (setf (ldb (byte 8 0) uint32) (read-byte instream))
    (setf (ldb (byte 8 8) uint32) (read-byte instream))
    (setf (ldb (byte 8 16) uint32) (read-byte instream))
    (setf (ldb (byte 8 24) uint32) (read-byte instream))
    uint32))

(defun make-uint32 (bytevec)
  (let ((uint32 0))
    (setf (ldb (byte 8 0) uint32) (elt bytevec 0))
    (setf (ldb (byte 8 8) uint32) (elt bytevec 1))
    (setf (ldb (byte 8 16) uint32) (elt bytevec 2))
    (setf (ldb (byte 8 24) uint32) (elt bytevec 3))
    uint32))

(defun read-uint64 (instream)
  (let ((uint64 0))
    (setf (ldb (byte 8 0) varint) (read-byte instream))
    (setf (ldb (byte 8 8) uint64) (read-byte instream))
    (setf (ldb (byte 8 16) uint64) (read-byte instream))
    (setf (ldb (byte 8 24) uint64) (read-byte instream))
    (setf (ldb (byte 8 32) uint64) (read-byte instream))
    (setf (ldb (byte 8 40) uint64) (read-byte instream))
    (setf (ldb (byte 8 48) uint64) (read-byte instream))
    (setf (ldb (byte 8 56) uint64) (read-byte instream))
    uint64))

(defun make-uint64 (bytevec)
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

(defun advance-file-position (fstream count)
  (file-position fstream (+ (file-position fstream) count)))

(defun copy-bytes (instream outstream count)
  (dotimes (i count)
    (write-byte (read-byte in) out)))

(defun make-blockfile-pathname (blocknum output-dir)
  (make-pathname
   :name (format nil "blk~7,'0d" blocknum)
   :type "dat"
   :defaults (pathname-as-directory output-dir)))
