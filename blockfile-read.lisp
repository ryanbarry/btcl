(in-package :btcl)

(defun parse-blockfile
    (&key (blockfile-pathname
	   #p"~/Library/Application Support/Bitcoin/blocks/blk00000.dat"))
  (make-block (file->bytevec blockfile-pathname)))

(defun file->bytevec (pathname)
  (with-open-file (finstream pathname :element-type '(unsigned-byte 8))
    (let* ((filelen (file-length finstream))
           (filevec (make-array filelen :element-type '(unsigned-byte 8))))
      (read-sequence filevec finstream)
      filevec)))
