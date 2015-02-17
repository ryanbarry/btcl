(in-package :btcl-db)

(defclass db-block ()
  ((hash :col-type (string 64) :initarg :hash)
   (network :col-type bigint :initarg :network)
   (version :col-type bigint :initarg :version)
   (hash-prev-block :col-type (string 64) :initarg :hash-prev-block)
   (hash-merkle-root :col-type (string 64) :initarg :hash-merkle-root)
   (timestamp :col-type timestamp-with-time-zone :initarg :timestamp)
   (bits :col-type bigint :initarg :bits)
   (nonce :col-type bigint :initarg :nonce)
   (height :col-type bigint :initarg :height))
  (:metaclass pomo:dao-class)
  (:keys hash)
  (:table-name block))

(pomo:deftable (db-block "block")
  (pomo:!dao-def))

(defclass db-transaction ()
  ((hash :col-type (string 64) :initarg :hash)
   (network :col-type bigint :initarg :network)
   (version :col-type bigint :initarg :version)
   (lock-time :col-type bigint :initarg :lock-time))
  (:metaclass pomo:dao-class)
  (:keys hash)
  (:table-name transaction))

(pomo:deftable (db-transaction "transaction")
  (pomo:!dao-def))

(defclass db-tx-in ()
  ((hash-prev-tx :col-type (string 64) :initarg :hash-prev-tx)
   (index-prev-tx :col-type integer :initarg :index-prev-tx)
   (script-sig :col-type bytea :initarg :script-sig)
   (sequence :col-type bigint :initarg :sequence)
   (transaction :col-type (string 64) :initarg :transaction)
   (tx-index :col-type integer :initarg :tx-index))
  (:metaclass pomo:dao-class)
  (:keys transaction tx-index)
  (:table-name tx-in))

(pomo:deftable (db-tx-in "tx-in")
  (pomo:!dao-def)
  (pomo:!foreign 'transaction 'transaction :primary-key))

(defclass db-tx-out ()
  ((value :col-type bigint :initarg :value)
   (script-pubkey :col-type bytea :initarg :script-pubkey)
   (transaction :col-type (string 64) :initarg :transaction)
   (tx-index :col-type integer :initarg :tx-index))
  (:metaclass pomo:dao-class)
  (:keys transaction tx-index)
  (:table-name tx-out))

(pomo:deftable (db-tx-out "tx-out")
  (pomo:!dao-def)
  (pomo:!foreign 'transaction 'transaction :primary-key))

(defclass db-block-transaction ()
  ((block-hash :col-type (string 64) :initarg :block-hash)
   (tx-hash :col-type (string 64) :initarg :tx-hash)
   (block-tx-index :col-type integer :initarg :block-tx-index))
  (:metaclass pomo:dao-class)
  (:keys block-hash block-tx-index)
  (:table-name block-transaction))

(pomo:deftable (db-block-transaction "block-transaction")
  (pomo:!dao-def)
  (pomo:!foreign 'block 'block-hash :primary-key)
  (pomo:!foreign 'transaction 'tx-hash :primary-key))

(defun start (&key (db "btcl") (user "btcl") (password "btcl") (host "localhost") (port 5432))
  (unless (and pomo:*database* (pomo:connected-p pomo:*database*))
    (pomo:connect-toplevel db user password host :port port))
  ;; TODO: auto-create schema if not already exists
  ;; (dolist (t '(blk transaction tx-in tx-out block-transaction)))
  )

(defun end ()
  (pomo:disconnect-toplevel))

(defgeneric save (obj))

(defmethod save ((obj bty:tx))
  (let ((tx-hash (btcl-digest:strhash obj)))
    (format t "hash: ~s (~d)~%" tx-hash (length tx-hash))
    (with-slots (bty::version bty::tx-in bty::tx-out bty::lock-time) obj
      (pomo:with-transaction ()
       (list (pomo:save-dao (make-instance 'db-transaction
                                           :hash tx-hash
                                           :network btcl-constants:+TESTNET3-MAGIC+
                                           :version bty::version
                                           :lock-time bty::lock-time))
             (loop for txin in bty::tx-in
                for i from 0
                collect (with-slots (bty::previous-output bty::script-sig bty::seq) txin
                          (pomo:save-dao (make-instance 'db-tx-in
                                                        :hash-prev-tx (ironclad:byte-array-to-hex-string
                                                                       (slot-value bty::previous-output 'bty::hash))
                                                        :index-prev-tx (slot-value bty::previous-output 'bty::index)
                                                        :script-sig bty::script-sig
                                                        :sequence bty::seq
                                                        :transaction tx-hash
                                                        :tx-index i))))
             (loop for txout in bty::tx-out
                for i from 0
                collect (with-slots (bty::value bty::script-pk) txout
                          (pomo:save-dao (make-instance 'db-tx-out
                                                        :value bty::value
                                                        :script-pubkey bty::script-pk
                                                        :transaction tx-hash
                                                        :tx-index i)))))))))

(defun init-db ()
  (pomo:create-package-tables 'btcl-db))
