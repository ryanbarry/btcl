(in-package :com-nuclearice-btcl)

(defparameter *local-bootstrap* #p"/Volumes/downloads/incomplete/bootstrap.dat")
(defparameter *local-blk-storage* #p"/Users/rbarry/projects/btcl/blocks")

(defun parse-bootstrap-file (&key (bootstrap-file *local-bootstrap*)
			       (output-dir *local-blk-storage*)
			       (numblocks 1))
  (with-open-file (in
		   bootstrap-file
		   :element-type '(unsigned-byte 8))
    (dotimes (blocknum numblocks)
      (let ((network (btclReadUint32 in))
	    (blocksize (btclReadUint32 in)))
	(format t "~3d of ~d blocks - net 0x~X size ~d~%" (1+ blocknum)
		numblocks network blocksize)
	(with-open-file (out
			 (make-pathname
			  :name (format nil "blk~7,'0d" blocknum)
			  :type "dat"
			  :defaults (pathname-as-directory output-dir))
			 :direction :output
			 :element-type '(unsigned-byte 8)
			 :if-does-not-exist :create
			 :if-exists :supersede)
	  (dotimes (i blocksize)
	    (write-byte (read-byte in) out)))))))

(defun btclReadUint32 (instream)
  (let ((uint32 0))
    (setf (ldb (byte 8 0) uint32) (read-byte instream))
    (setf (ldb (byte 8 8) uint32) (read-byte instream))
    (setf (ldb (byte 8 16) uint32) (read-byte instream))
    (setf (ldb (byte 8 24) uint32) (read-byte instream))
    uint32))
