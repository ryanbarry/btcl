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
	    (with-open-file (out
			     (make-pathname
			      :name (format nil "blk~7,'0d" blocknum)
			      :type "dat"
			      :defaults (pathname-as-directory output-dir))
			     :direction :output
			     :element-type '(unsigned-byte 8)
			     :if-does-not-exist :create
			     :if-exists :supersede)
	      (copy-bytes in out blocksize)
	      (format t " *~%")))))))

(defun read-uint32 (instream)
  (let ((uint32 0))
    (setf (ldb (byte 8 0) uint32) (read-byte instream))
    (setf (ldb (byte 8 8) uint32) (read-byte instream))
    (setf (ldb (byte 8 16) uint32) (read-byte instream))
    (setf (ldb (byte 8 24) uint32) (read-byte instream))
    uint32))

(defun advance-file-position (fstream count)
  (file-position fstream (+ (file-position fstream) count)))

(defun copy-bytes (instream outstream count)
  (dotimes (i count)
    (write-byte (read-byte in) out)))
