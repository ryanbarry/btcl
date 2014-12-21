;;; credit goes to Common Lisp Tips
;;; http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs
(in-package :com-nuclearice-btcl)

(defvar +unix-epoch-difference+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time +unix-epoch-difference+))

(defun unix-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-difference+))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))
