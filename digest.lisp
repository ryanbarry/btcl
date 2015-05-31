(in-package :btcl-digest)

(defun dsha256-checksum (data)
  (let ((hash (dsha256 data))
        (checksum 0))
    (setf (ldb (byte 8 0) checksum) (elt hash 0))
    (setf (ldb (byte 8 8) checksum) (elt hash 1))
    (setf (ldb (byte 8 16) checksum) (elt hash 2))
    (setf (ldb (byte 8 24) checksum) (elt hash 3))
    (values checksum
            hash)))

(defun dsha256 (data)
  (let ((digester (ironclad:make-digest :sha256)))
    (ironclad:update-digest digester data)
    (let ((1st-round (ironclad:produce-digest digester)))
      (reinitialize-instance digester)
      (ironclad:update-digest digester 1st-round)
      (ironclad:produce-digest digester))))

(defgeneric hash (obj))

(defmethod hash ((obj bty:tx))
  (let ((octstream (ironclad:make-octet-output-stream)))
    (bindata:write-value 'bty:tx octstream obj)
    (btcl-digest:dsha256 (ironclad:get-output-stream-octets octstream))))

(defmethod hash ((obj bty:blk))
  (let ((octstream (ironclad:make-octet-output-stream)))
    (bindata:write-value 'bty:blk octstream obj)
    (btcl-digest:dsha256 (ironclad:get-output-stream-octets octstream))))

(defun strhash (obj)
  (ironclad:byte-array-to-hex-string (hash obj)))
