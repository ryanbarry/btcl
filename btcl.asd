(asdf:defsystem #:btcl
  :serial t
  :depends-on (#:cl-async #:binary-types #:ironclad)
  :components ((:file "package")
               (:file "portable-pathnames")
               (:file "time")
               (:file "block")
               (:file "blockfile-read")
               (:file "net")
               (:file "digest")))
