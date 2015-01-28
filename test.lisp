(in-package :btcl-net)

(let* ((msg (make-instance 'msg-version
                           :version 70002
                           :services 1
                           :timestamp (get-unix-time)
                           :addr-recv (make-instance 'version-net-addr
                                                :services 1
                                                :ip-addr (build-ip-addr 10 0 1 55)
                                                :port 18333)
                           :addr-from (make-instance 'version-net-addr
                                                :services 1
                                                :ip-addr (build-ip-addr 10 0 1 185)
                                                :port 18333)
                           :nonce (random (expt 2 64))
                           :user-agent (make-varstr "/btcl:0.0.1/")
                           :start-height 0
                           :relay 1))
       (msg-bytes (let ((vecstream (ironclad:make-octet-output-stream)))
                    (bindata:write-value 'msg-version vecstream msg)
                    (ironclad:get-output-stream-octets vecstream)))
       (msg-header (make-instance 'header
                                  :magic +TESTNET3-MAGIC+
                                  :command "version"
                                  :len (length msg-bytes)
                                  :checksum (dsha256-checksum msg-bytes))))
  (with-open-file (f "testmsg-header.bin"
                     :element-type '(unsigned-byte 8)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
    (bindata:write-value 'header f msg-header))
  (with-open-file (f "testmsg-payload.bin"
                     :element-type '(unsigned-byte 8)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
    (bindata:write-value 'msg-version f msg))
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
      (bindata:write-value 'header socket msg-header)
      (bindata:write-value 'msg-version socket msg))))
