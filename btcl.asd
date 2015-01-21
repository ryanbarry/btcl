(asdf:defsystem #:btcl
  :serial t
  :depends-on (#:cl-async #:binary-types)
  :components ((:file "package")
               (:file "portable-pathnames")
               (:file "time")
               (:file "block")
               (:file "blockfile-read")
               (:file "net")))
