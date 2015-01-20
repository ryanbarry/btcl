(in-package :btcl)

(defparameter *blockpointer* nil)

(defun print-block (blk)
  (format t "v: ~d~%" (slot-value blk 'version))
  (format t "hash prev: ~X~%" (slot-value blk 'hash-prev-block))
  (format t "merkle root: ~X~%" (slot-value blk 'hash-merkle-root))
  (format t "time: ~d~%" (slot-value blk 'time))
  (format t "bits: 0x~X~%" (slot-value blk 'bits))
  (format t "nonce: ~d~%" (slot-value blk 'nonce))
  (loop
     for tx in (slot-value blk 'vtx)
     for i = 1 then (incf i)
     do
       (format t "~ttxno: ~d~%" 1)
       (format t "~tv: ~d~%" (slot-value tx 'version))
       (loop for txin in (slot-value tx 'vin)
          for i = 1 then (incf i)
          do (format t "~t~tinput: ~d~%" i)
            (format t "~t~t~thash: ~X~%" (slot-value txin 'hash))
            (format t "~t~t~tn: ~d~%" (slot-value txin 'n))
            (format t "~t~t~tscriptSig: ~X~%" (slot-value txin 'script-sig))
            (format t "~t~t~tsequence: ~d~%" (slot-value txin 'sequence)))
       (loop for txout in (slot-value tx 'vout)
          for i = 1 then (incf i)
          do (format t "~t~toutput: ~d~%" i)
            (format t "~t~t~tvalue: ~d~%" (slot-value txout 'value))
            (format t "~t~t~tscriptPubkey: ~X~%" (slot-value txout 'script-pubkey)))
       (format t "~ttxLockTime: ~d~%" (slot-value tx 'lock-time))))

(defun parse-blockfile
    (&key (blockfile-pathname
	   #p"~/Library/Application Support/Bitcoin/blocks/blk00000.dat"))
  (let* ((blockfile (file->bytevec blockfile-pathname))
         (*blockpointer* 0))
    (format t "network 0x~X and block size ~d~%"
            (read-uint32 blockfile)
            (read-uint32 (subseq blockfile *blockpointer*)))
    (make-instance
     'block
     :ver (read-uint32 (subseq blockfile *blockpointer*))
     :hash-prev (subseq blockfile *blockpointer* (incf *blockpointer* 32))
     :merkle-root (subseq blockfile *blockpointer* (incf *blockpointer* 32))
     :time (read-uint32 (subseq blockfile *blockpointer*))
     :bits (read-uint32 (subseq blockfile *blockpointer*))
     :nonce (read-uint32 (subseq blockfile *blockpointer*))
     :vtx (loop for txno from 1 upto (read-varint (subseq blockfile *blockpointer*))
             ;;(format t "~ttxno: ~d~%" txno)
             collect (make-instance 'transaction
                                    :ver (read-uint32 (subseq blockfile *blockpointer*))
                                    :vin (loop for inpno from 1 to (read-varint (subseq blockfile *blockpointer*))
                                             ;;(format t "~t~tinput no: ~d~%" inpno)
                                             collect (make-instance 'txin
                                                                   :hash (subseq blockfile *blockpointer* (incf *blockpointer* 32))
                                                                   :n (read-uint32 (subseq blockfile *blockpointer*))
                                                                   :script-sig (subseq blockfile
                                                                                       *blockpointer*
                                                                                       (incf *blockpointer*
                                                                                             (read-varint (subseq blockfile *blockpointer*))))
                                                                   :seq (read-uint32 (subseq blockfile *blockpointer*))))
                                    :vout (loop for outno from 1 to (read-varint (subseq blockfile *blockpointer*))
                                             ;;(format t "~t~toutput no: ~d~%" outno)
                                             collect (make-instance 'txout
                                                                    :val (/ (read-uint64 (subseq blockfile *blockpointer*)) 100000000)
                                                                    :script-pubkey (subseq blockfile
                                                                                           *blockpointer*
                                                                                           (incf *blockpointer*
                                                                                                 (read-varint (subseq blockfile *blockpointer*))))))
                                    :lock-time (read-uint32 (subseq blockfile *blockpointer*)))))))

(defun parse-tx (tx-bytes)
  )

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
