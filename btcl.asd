(asdf:defsystem #:btcl
  :serial t
  :depends-on (#:cl-async #:ironclad)
  :components ((:file "package")
               (:file "macro-utilities")
               (:file "portable-pathnames")
               (:file "binary-data")
               (:file "time")
               (:file "block")
               (:file "blockfile-read")
               (:file "digest")
               (:file "datatypes")
               (:file "net")
               (:file "peer")))
