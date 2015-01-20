(asdf:defsystem #:btcl
  :serial t
;;  :depends-on ()
  :components ((:file "package")
               (:file "portable-pathnames")
               (:file "time")
               (:file "block")
               (:file "blockfile-read")))
