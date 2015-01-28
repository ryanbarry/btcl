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
                                  :magic +TESTNET3-MAGIC+
                                  :command "version"
                                  :len (length msg-bytes)
                                  :checksum (dsha256-checksum msg-bytes))))
  (binary-types:with-binary-file (f "testmsg-header.bin"
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :overwrite)
    (binary-types:write-binary 'header f msg-header))
  (binary-types:with-binary-file (f "testmsg-payload.bin"
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :overwrite)
    (binary-types:write-binary 'msg-version f msg))
  (cl-async:with-event-loop (:catch-app-errors t)
    (let ((socket (cl-async:tcp-connect "10.0.1.55"
                                        18333
                                        (lambda (socket stream)
                                          (with-open-file (f "test-responses.bin"
                                                             :direction :output
                                                             :element-type '(unsigned-byte 8)
                                                             :if-does-not-exist :create
                                                             :if-exists :append)
                                            (loop for b = (read-byte stream nil 'eof)
                                               for i = 1 then (incf i)
                                               until (equalp 'eof b)
                                               do (write-byte b f))))
                                        (lambda (event)
                                          (format t "ev: ~a~%" event))                            
                                        :stream t)))
      (binary-types:write-binary 'header socket msg-header)
      (binary-types:write-binary 'msg-version socket msg))))
