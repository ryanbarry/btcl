(asdf:defsystem #:btcl
  :serial t
  :depends-on (#:cl-async #:binary-types #:ironclad #:swap-bytes)
  :components ((:file "package")
               (:file "macro-utilities")
               (:file "portable-pathnames")
               (:file "binary-data")
               (:file "time")
               (:file "block")
               (:file "blockfile-read")
               (:file "digest")
               (:file "net")))
