(in-package :btcl-net)

;; network byte order
(defconstant +TESTNET3-MAGIC+ #x0709110b)

(binary-types:define-unsigned u64 8)
(binary-types:define-signed s64 8)
(binary-types:define-binary-string header-command 12)
(binary-types:define-unsigned net-addr-ip-addr 16)
(binary-types:define-binary-class version-net-addr ()
  ((services :binary-type 'u64
             :initarg :sv)
   (ip-addr :binary-type net-addr-ip-addr
            :initarg :ip)
   (port :binary-type binary-types:u16
         :initarg :port
         :map-binary-write (lambda (value type-name)
                             (swap-bytes:swap-bytes-16 value)))))

(binary-types:define-binary-class header ()
  ((magic :binary-type binary-types:u32
          :initarg :magic)
   (command :binary-type header-command
            :initarg :command)
   (length :binary-type binary-types:u32
           :initarg :len)
   (checksum :binary-type binary-types:u32
             :initarg :checksum)))

(binary-types:define-binary-class msg-version ()
  ((version :binary-type binary-types:s32
            :initarg :ver)
   (services :binary-type 'u64
             :initarg :sv)
   (timestamp :binary-type 's64
              :initarg :ts)
   (addr-recv :binary-type version-net-addr
              :initarg :recv)
   (addr-from :binary-type version-net-addr
              :initarg :from)
   (nonce :binary-type 'u64
          :initarg :n)
   (user-agent :binary-type binary-types:char8
               :initarg :ua)
   (start-height :binary-type binary-types:s32
                 :initarg :height)
   (relay :binary-type binary-types:u8
          :initarg :relay)))

(defun build-ip-addr (&rest addr)
  "make a 16-byte address in network byte order"
  (if (or (< (length addr) 4) (some #'> addr '(255 255 255 255)))
      (error "You must specify the address as 4 numbers, e.g. 192 168 1 10")
      (let ((result 0))
        (setf (ldb (byte 8 80) result) 255) ;;;; 0xFFFF to represent IPv4
        (setf (ldb (byte 8 88) result) 255) ;;;; address within IPv6 address
        (setf (ldb (byte 8 96) result) (elt addr 0))
        (setf (ldb (byte 8 104) result) (elt addr 1))
        (setf (ldb (byte 8 112) result) (elt addr 2))
        (setf (ldb (byte 8 120) result) (elt addr 3))
        result)))
