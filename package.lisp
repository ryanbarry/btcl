;;;
;;; BTCL: Bitcoin network node implementation in Common Lisp
;;; Copyright (C) 2015 Ryan Barry <ryan@nuclearice.com>
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

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
  (:export #:parse-bootstrap-file
           #:parse-blockfile
           #:print-block
           #:get-unix-time))

(defpackage #:btcl-digest
  (:use #:cl #:btcl)
  (:export #:dsha256-checksum))

(defpackage #:btcl-net
  (:use #:cl #:btcl #:btcl-digest)
  (:shadow :block)
  (:export #:build-ip-addr
           #:start-peer))
