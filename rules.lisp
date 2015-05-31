(in-package :btcl-rules)

(defun check-block-header (blk-header)
  (if (and (check-proof-of-work (hash blk-header) (bits block-header))
           (check-block-timestamp (timestamp blk-header)))
      t
      nil))

(defun check-proof-of-work (hash bits)
  ;; bits->target calculation from https://en.bitcoin.it/wiki/Difficulty#How_is_difficulty_stored_in_blocks.3F
  (if (<= hash (* bits (expt 2 (* 8 (- #x1b 3)))))
      t
      nil))

(defun check-block-timestamp (timestamp)
  (if (<= timestamp (+ (get-unix-time) (* 2 60 60)))
      t
      nil))
