(in-package :cl-user)

(defpackage #:com.gigamonkeys.portable-pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

(defpackage #:btcl
  (:import-from #:com.gigamonkeys.portable-pathnames #:pathname-as-directory)
  (:use #:cl)
  (:shadow :block)
  (:export
   #:parse-bootstrap-file
   #:parse-blockfile
   #:print-block
   #:get-unix-time))

(defpackage #:btcl-digest
  (:use #:cl #:btcl)
  (:export #:dsha256-checksum))

(defpackage #:btcl-net
  (:use #:cl #:btcl #:btcl-digest)
  (:export #:build-ip-addr))
