(in-package :com-nuclearice-btcl)

(defparameter +current-block-version+ 2)

(defclass block ()
  ((version
    :initarg :ver
    :initform +current-block-version+)
   (hash-prev-block
    :initarg :hash-prev
    :initform nil)
   (hash-merkle-root
    :initarg :merkle-root
    :initform nil)
   (time
    :initarg :time
    :initform (get-unix-time))
   (bits
    :initarg :bits)
   (nonce
    :initarg :nonce)
   (vtx
    :initarg :vtx)))

