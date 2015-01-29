(in-package :cl-user)

(defpackage #:com.gigamonkeys.macro-utilities
  (:nicknames :macut)
  (:use :common-lisp)
  (:export 
   :with-gensyms
   :with-gensymed-defuns
   :once-only
   :spliceable
   :ppme))

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

(defpackage #:com.gigamonkeys.binary-data
  (:nicknames :bindata)
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :+null+))

(defpackage #:btcl
  (:import-from #:com.gigamonkeys.portable-pathnames #:pathname-as-directory)
  (:use #:cl)
  (:shadow :block)
  (:export #:parse-bootstrap-file
           #:parse-blockfile
           #:print-block
           #:get-unix-time))

(defpackage #:btcl-digest
  (:use #:cl #:btcl)
  (:export #:dsha256-checksum))

(defpackage #:btcl-net
  (:use #:cl #:btcl #:btcl-digest)
  (:export #:build-ip-addr
           #:start-peer))
