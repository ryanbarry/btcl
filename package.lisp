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

(defpackage #:btcl-web
  (:use :cl :cl-ppcre :cl-async :cl-who :parenscript)
  (:import-from :babel #:octets-to-string)
  (:export #:start-server
           #:publish!
           #:notify-tx
           #:notify-blk))

(defpackage #:btcl-constants
  (:nicknames #:btconst)
  (:use #:cl)
  (:export #:+P2P-MSG-HEADER-LEN+
           #:+TESTNET3-MAGIC+
           #:+TESTNET3-GENESIS-BLOCK+
           #:+MAINNET-MAGIC+
           #:+MAINNET-GENESIS-BLOCK+))


(defpackage #:btcl-types
  (:nicknames #:bty)
  (:use #:cl)
  (:export #:raw-bytes
           #:u8
           #:u16be
           #:u32be
           #:u64be
           #:u16le
           #:u32le
           #:u64le
           #:s32le
           #:s64le
           #:varint
           #:iso-8859-1-string
           #:varstr
           #:net-addr
           #:version-net-addr
           #:addr-list
           #:inv-vector
           #:inv-vector-list
           #:outpoint
           #:tx-in
           #:tx-in-list
           #:tx-out
           #:tx-out-list
           #:tx
           #:tx-list
           #:blk
           #:build-ip-addr))

(defpackage #:btcl-digest
  (:use #:cl)
  (:export #:dsha256-checksum
           #:dsha256
           #:hash
           #:strhash))

(defpackage #:btcl-db
  (:use #:cl #:postmodern #:btcl-types)
  (:export #:start
           #:end
           #:init-db
           #:save))

(defpackage #:btcl-wire
  (:nicknames #:btw)
  (:use #:cl #:btcl #:btcl-digest #:btcl-util #:btcl-types #:btcl-db)
  (:export #:peer-connection
           #:start-peer
           #:msg-version
           #:msg-verack
           #:msg-inv
           #:msg-getdata
           #:msg-tx
           #:msg-block
           #:msg-addr))

