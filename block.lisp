(in-package :com-nuclearice-btcl)

(defparameter +current-block-version+ 2) ; v2 since BIP0034
(defparameter +current-txn-version+ 1)

(defclass block ()
  ((version ; int32
    :initarg :ver
    :initform +current-block-version+)
   (hash-prev-block ; uint256
    :initarg :hash-prev
    :initform nil)
   (hash-merkle-root ; uint256
    :initarg :merkle-root
    :initform nil)
   (time ; uint32
    :initarg :time
    :initform (get-unix-time))
   (bits ; uint32
    :initarg :bits)
   (nonce ; uint32
    :initarg :nonce)
   (vtx ; vector<transaction>
    :initarg :vtx)))

(defclass transaction ()
  ((version ; int32
    :initarg :ver
    :initform +current-txn-version+)
   (vin ; vector<txin>
    :initarg :vin)
   (vout ; vector<txout>
    :initarg :vout)
   (lock-time ; uint32
    :initarg :lock-time
    :initform (get-unix-time))))
