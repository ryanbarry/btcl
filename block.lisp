(in-package :btcl-core)

(defclass blok-header ()
  ((version :accessor version)
   (hash-prev-block :accessor hash-prev-block)
   (hash-merkle-root :accessor hash-merkle-root)
   (timestamp :accessor timestamp)
   (bits :accessor bits)
   (nonce :accessor nonce)
   (hash :accessor hash)))

(defclass blok ()
  ((header :accessor header)
   (tx-list :accessor tx-list)))
