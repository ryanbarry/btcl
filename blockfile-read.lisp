(in-package :btcl)

(defparameter *blockpointer* nil)

(defun print-block (blk)
  (format t "v: ~d~%" (slot-value blk 'version))
  (format t "hash prev: ~X~%" (slot-value blk 'hash-prev-block))
  (format t "merkle root: ~X~%" (slot-value blk 'hash-merkle-root))
  (format t "time: ~d~%" (slot-value blk 'time))
  (format t "bits: 0x~X~%" (slot-value blk 'bits))
  (format t "nonce: ~d~%" (slot-value blk 'nonce))
  ;;(format t "txLockTime: ~d~%" (read-uint32 (subseq blockfile *blockpointer*)))
  )

(defun parse-blockfile
    (&key (blockfile-pathname
	   #p"~/Library/Application Support/Bitcoin/blocks/blk00000.dat"))
  (let* ((blockfile (file->bytevec blockfile-pathname))
         (*blockpointer* 0))
    (format t "network 0x~X and block size ~d"
            (read-uint32 blockfile)
            (read-uint32 blockfile))
    (make-instance 'block
                   :ver (read-uint32 blockfile)
                   :hash-prev (subseq blockfile *blockpointer*
                                      (incf *blockpointer* 32))
                   :merkle-root (subseq blockfile *blockpointer*
                                        (incf *blockpointer* 32))
                   :time (read-uint32 (subseq blockfile *blockpointer*))
                   :bits (read-uint32 (subseq blockfile *blockpointer*))
                   :nonce (read-uint32 (subseq blockfile *blockpointer*))
                   :vtx (vector))
    ;; (dotimes (txno (read-varint (subseq blockfile *blockpointer*)))
    ;;   (format t "~ttx no: ~d~%~tv: ~d~%" txno
    ;;           (read-uint32 (subseq blockfile *blockpointer*)))
    ;;   (dotimes (inpno (read-varint (subseq blockfile *blockpointer*)))
    ;;     (format t "~t~tinput no: ~d~%" inpno)
    ;;     (format t "~t~t~thash: ~X~%"
    ;;     	(subseq blockfile *blockpointer* (incf *blockpointer* 32)))
    ;;     (format t "~t~t~tidx: ~d~%"
    ;;     	(read-uint32 (subseq blockfile *blockpointer*)))
    ;;     (let ((scriptlen (read-varint (subseq blockfile *blockpointer*))))
    ;;       (format t "~t~t~tscript: ~X~%"
    ;;     	  (subseq blockfile *blockpointer* (incf *blockpointer* scriptlen))))
    ;;     (format t "~t~t~tseq: 0x~X~%"
    ;;     	(read-uint32 (subseq blockfile *blockpointer*))))
    ;;   (dotimes (outno (read-varint (subseq blockfile *blockpointer*)))
    ;;     (format t "~t~toutput no: ~d~%" outno)
    ;;     (format t "~t~t~tval: ~f~%"
    ;;     	(/ (read-uint64 (subseq blockfile *blockpointer*)) 100000000))
    ;;     (let ((scriptlen (read-varint (subseq blockfile *blockpointer*))))
    ;;       (format t "~t~t~tscript: ~X~%"
    ;;     	  (subseq blockfile *blockpointer* (incf *blockpointer* scriptlen))))))
    ))

(defun file->bytevec (pathname)
  (with-open-file (finstream pathname :element-type '(unsigned-byte 8))
    (let* ((filelen (file-length finstream))
           (filevec (make-array filelen :element-type '(unsigned-byte 8))))
      (read-sequence filevec finstream)
      filevec)))

(defun read-uint16 (bytevec)
  (let ((uint16 0))
    (setf (ldb (byte 8 0) uint16) (elt bytevec 0))
    (setf (ldb (byte 8 8) uint16) (elt bytevec 1))
    (incf *blockpointer* 2)
    uint16))

(defun read-uint32 (bytevec)
  (let ((uint32 0))
    (setf (ldb (byte 8 0) uint32) (elt bytevec 0))
    (setf (ldb (byte 8 8) uint32) (elt bytevec 1))
    (setf (ldb (byte 8 16) uint32) (elt bytevec 2))
    (setf (ldb (byte 8 24) uint32) (elt bytevec 3))
    (incf *blockpointer* 4)
    uint32))

(defun read-uint64 (bytevec)
  (let ((uint64 0))
    (setf (ldb (byte 8 0) uint64) (elt bytevec 0))
    (setf (ldb (byte 8 8) uint64) (elt bytevec 1))
    (setf (ldb (byte 8 16) uint64) (elt bytevec 2))
    (setf (ldb (byte 8 24) uint64) (elt bytevec 3))
    (setf (ldb (byte 8 32) uint64) (elt bytevec 4))
    (setf (ldb (byte 8 40) uint64) (elt bytevec 5))
    (setf (ldb (byte 8 48) uint64) (elt bytevec 6))
    (setf (ldb (byte 8 56) uint64) (elt bytevec 7))
    (incf *blockpointer* 8)
    uint64))

(defun read-varint (bytevec)
  (let ((varint (elt bytevec 0)))
    (if (< varint #xFD)
	(progn
	  (incf *blockpointer*)
	  varint)
	(cond ((= varint #xFD) ; 1st byte == 0xFD, then uint16_t
	       (read-uint16 bytevec))
	      ((= varint #xFE) ; 1st byte == 0xFE, then uint32_t
	       (read-uint32 bytevec))
	      ((= varint #xFF) ; 1st byte == 0xFF, then uint64_t
	       (read-uint64 bytevec))))))
