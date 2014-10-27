(defun parse-bootstrap-file ()
  (let ((bootstrap-file-pathname "/Volumes/downloads/incomplete/bootstrap.dat"))
    (with-open-file (bootstrap-file
		     bootstrap-file-pathname
		     :element-type '(unsigned-byte 8))
      (let ((network (btclReadUint32 bootstrap-file))
	    (blocksize (btclReadUint32 bootstrap-file)))
	(format t "writing ~d byte block from net 0x~X" blocksize network)
	(with-open-file (blk-output
			 "/Users/rbarry/projects/btcl/blocks/blk0000000.dat.2"
			 :direction :output
			 :element-type '(unsigned-byte 8))
	  (dotimes (i blocksize)
	    (write-byte (read-byte bootstrap-file) blk-output)))))))

(defun btclReadUint32 (instream)
  (let ((uint32 0))
    (setf (ldb (byte 8 0) uint32) (read-byte instream))
    (setf (ldb (byte 8 8) uint32) (read-byte instream))
    (setf (ldb (byte 8 16) uint32) (read-byte instream))
    (setf (ldb (byte 8 24) uint32) (read-byte instream))
    uint32))
