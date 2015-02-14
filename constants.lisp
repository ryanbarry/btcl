(in-package :btcl-constants)

(defconstant +P2P-MSG-HEADER-LEN+ 24)

(defconstant +TESTNET3-MAGIC+ #x0709110b) ; network byte order
(defconstant +MAINNET-MAGIC+ #xfeb4bef9) ; network byte order

(defconstant +MAINNET-GENESIS-BLOCK+ (make-array 285 :element-type '(unsigned-byte 8) :initial-contents
                                                 #(#x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                                                   #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                                                   #x00 #x00 #x00 #x00 #x3B #xA3 #xED #xFD  #x7A #x7B #x12 #xB2 #x7A #xC7 #x2C #x3E
                                                   #x67 #x76 #x8F #x61 #x7F #xC8 #x1B #xC3  #x88 #x8A #x51 #x32 #x3A #x9F #xB8 #xAA
                                                   #x4B #x1E #x5E #x4A #x29 #xAB #x5F #x49  #xFF #xFF #x00 #x1D #x1D #xAC #x2B #x7C
                                                   #x01 #x01 #x00 #x00 #x00 #x01 #x00 #x00  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                                                   #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                                                   #x00 #x00 #x00 #x00 #x00 #x00 #xFF #xFF  #xFF #xFF #x4D #x04 #xFF #xFF #x00 #x1D
                                                   #x01 #x04 #x45 #x54 #x68 #x65 #x20 #x54  #x69 #x6D #x65 #x73 #x20 #x30 #x33 #x2F
                                                   #x4A #x61 #x6E #x2F #x32 #x30 #x30 #x39  #x20 #x43 #x68 #x61 #x6E #x63 #x65 #x6C
                                                   #x6C #x6F #x72 #x20 #x6F #x6E #x20 #x62  #x72 #x69 #x6E #x6B #x20 #x6F #x66 #x20
                                                   #x73 #x65 #x63 #x6F #x6E #x64 #x20 #x62  #x61 #x69 #x6C #x6F #x75 #x74 #x20 #x66
                                                   #x6F #x72 #x20 #x62 #x61 #x6E #x6B #x73  #xFF #xFF #xFF #xFF #x01 #x00 #xF2 #x05
                                                   #x2A #x01 #x00 #x00 #x00 #x43 #x41 #x04  #x67 #x8A #xFD #xB0 #xFE #x55 #x48 #x27
                                                   #x19 #x67 #xF1 #xA6 #x71 #x30 #xB7 #x10  #x5C #xD6 #xA8 #x28 #xE0 #x39 #x09 #xA6
                                                   #x79 #x62 #xE0 #xEA #x1F #x61 #xDE #xB6  #x49 #xF6 #xBC #x3F #x4C #xEF #x38 #xC4
                                                   #xF3 #x55 #x04 #xE5 #x1E #xC1 #x12 #xDE  #x5C #x38 #x4D #xF7 #xBA #x0B #x8D #x57
                                                   #x8A #x4C #x70 #x2B #x6B #xF1 #x1D #x5F  #xAC #x00 #x00 #x00 #x00)))



