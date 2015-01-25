(in-package :btcl-net)

(let* ((binary-types:*endian* :little-endian)
       (msg (make-instance 'msg-version
                           :ver 70002
                           :sv 1
                           :ts (get-unix-time)
                           :recv (make-instance 'version-net-addr
                                                :sv 1
                                                :ip (build-ip-addr 10 0 1 55)
                                                :port 18333)
                           :from (make-instance 'version-net-addr
                                                :sv 1
                                                :ip (build-ip-addr 10 0 1 185)
                                                :port 18333)
                           :n (random (expt 2 64))
                           :ua #\null
                           :height 0
                           :relay 1))
       (msg-bytes (let ((vec (make-array 1 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
                    (binary-types:with-binary-output-to-vector (stream vec :adjustable t)
                      (binary-types:write-binary 'msg-version stream msg))
                    (make-array (fill-pointer vec) :element-type '(unsigned-byte 8) :initial-contents vec)))
       (msg-header (make-instance 'header
                                  :magic (make-header-magic-number
                                          :mag0 (elt +TESTNET3-MAGIC+ 0)
                                          :mag1 (elt +TESTNET3-MAGIC+ 1)
                                          :mag2 (elt +TESTNET3-MAGIC+ 2)
                                          :mag3 (elt +TESTNET3-MAGIC+ 3))
                                  :command "version"
                                  :len (length msg-bytes)
                                  :checksum (dsha256-checksum msg-bytes))))
  (binary-types:with-binary-file (f "testv.bin"
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :overwrite)
    (binary-types:write-binary 'header f msg-header)
    (binary-types:write-binary 'msg-version f msg))))


