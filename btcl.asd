(asdf:defsystem #:btcl
  :serial t
  :depends-on (#:cl-async #:binary-types #:ironclad #:swap-bytes)
  :components ((:file "package")
               (:file "portable-pathnames")
               (:file "time")
               (:file "block")
               (:file "blockfile-read")
               (:file "digest")
               (:file "net")))
