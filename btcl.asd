(asdf:defsystem #:btcl
  :serial t
  :depends-on (#:cl-async
               #:ironclad
               #:babel
               #:cl-who
               #:cl-ppcre
               #:parenscript
               #:cl-json
               #:postmodern)
  :components ((:file "package")
               (:file "macro-utilities")
               (:file "portable-pathnames")
               (:file "binary-data")
               (:file "time")
               (:file "web")
               (:file "constants")
               (:file "digest")
               (:file "datatypes")
               (:file "db")
               (:file "wire")
               (:file "peer")))
