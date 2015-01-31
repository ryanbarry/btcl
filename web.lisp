;;; from http://pastebin.com/yW6Xbuv1

(in-package :btcl-web)

(defparameter *channel* nil)
 
(defun start-server ()
  (format t "Starting Web UI server.~%")
  (as:tcp-server "0.0.0.0" 3000
                 (lambda (socket data)
                   (let ((req (parse (octets-to-string data))))
                     (format t "~s~%~%" req)
                     (cond ((string= "/sub" req)
                            (subscribe! socket))
                           ((string= "/pub" req)
                            (publish! "Got a message!")
                            (write/close socket (cat "HTTP/1.1 200 OK" crlf
                                                     "Content-Type: text/plain; charset=UTF-8" crlf
                                                     "Cache-Control: no-cache, no-store, must-revalidate" crlf
                                                     "Content-Length: 10" crlf crlf
                                                     "Published!" crlf crlf)))
                           ((string= "/watch" req)
                            (write/close socket test-page))
                           (t
                            (write/close socket (cat "HTTP/1.1 200 OK" crlf
                                                     "Content-Type: text/plain; charset=UTF-9" crlf
                                                     "Content-Length: 2" crlf crlf
                                                     "Ok" crlf crlf))))))
                 :event-cb (lambda (err)
                             (format t "listener event: ~a" err))))
 
(defparameter day-names '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
 
(defun http-date ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~a, ~a ~a ~a ~a:~a:~a GMT~@d"
            (nth day-of-week day-names) date
            (nth month month-names) year hour minute second (- tz))))
 
(defun subscribe! (sock)
  (write/keep-alive
   sock (cat "HTTP/1.1 200 OK" crlf
             "Date: " (http-date) crlf
             "Content-Type: text/event-stream; charset=utf-8" crlf
             "Transfer-Encoding: chunked" crlf
             "Connection: keep-alive" crlf
             "Expires: Thu, 01 Jan 1970 00:00:01 GMT" crlf
             "Cache-Control: no-cache, no-store, must-revalidate" crlf crlf))
  (push sock *channel*)
  (write/keep-alive sock (cat ": comment" crlf crlf)))
 
(defun publish! (msg)
;  (format t "~&sending msg: ~s~%" msg)
  (loop for sock in *channel*
     do (handler-case
            (if (as:socket-closed sock)
                (setf *channel* (remove sock *channel*))
                (write/keep-alive sock (format nil "data: ~a~c~c~c~c" msg #\Return #\Linefeed #\Return #\Linefeed)))
          (error (e)
            (declare (ignore e))
            (format t "Removing inactive socket...")
            (setf *channel* (remove sock *channel*))))))
 
(defun write/keep-alive (sock data)
  (unless (as:socket-closed sock)
    (as:write-socket-data sock (cat (format nil "~X" (length data)) crlf data crlf))))
 
(defun write/close (sock data)
  (unless (as:socket-closed sock)
    (as:write-socket-data
     sock data
     :write-cb (lambda (sock)
                 (setf (as:socket-data sock) nil)
                 (as:close-socket sock)))))
 
(defmethod parse ((req string))
  (let ((lines (split "\\r?\\n" req)))
    (second (split " " (first lines)))))
 
;;;;;;;;;; General purpose utilities
(defun cat (&rest seqs)
  (apply #'concatenate 'string seqs))
(defconstant crlf (list #\return #\linefeed))
(defparameter test-page
  (let ((content (with-html-output-to-string (str nil :prologue t)
                   (:html
                    (:head (:title "Live Network Activity"))
                    (:body
                     (:div :id "row" :style "margin-right: -15px; margin-left: -15px; clear:both;"
                           (:div :id "row" :style "margin-right: -15px; margin-left: -15px;"
                                 (:div :id "txn-container" :style "float: left; width: 33%; padding-left: 15px; padding-right: 15px; position:relative;"
                                       (:h3 :style "width: 100%; display: inline; padding-left: 15px;" "Transactions"))
                                 (:div :id "block-container" :style "width:66%; padding-left: 15px; padding-right: 15px; position:relative;"
                                       (:h3 :style "width: 100%; display: inline; padding-left: 15px;" "Blocks"))))
                     (:div)
                     (:div :id "row" :style "margin-right: -15px; margin-left: -15px; clear: both; min-height: 600px; width:95%;"
                           (:div :id "txn-container" :style "float: left; width: 33%; padding-left: 15px; padding-right: 15px; position:relative;"
                                 (:table :id "txn-list" :style "width: 100%; border: 1px solid black;"
                                         (:thead :style "color: white; background-color: #333;"
                                          (:tr
                                           (:th "Latest TX") (:th "In : Out") (:th "Sent (BTC)")))
                                         (:tbody)))
                           (:div :id "block-container" :style "width:66%; padding-left: 15px; padding-right: 15px; position:relative;"
                                 (:table :id "blk-list" :style "width: 100%; border: 1px solid black;"
                                         (:thead :style "color: white; background-color: #333;"
                                          (:tr
                                           (:th "Time") (:th "Txns") (:th "Difficulty")))
                                         (:tbody))))
                     (:div)
                     (:div :id "console")
                     (:script :src "//code.jquery.com/jquery-1.11.2.min.js")
                     (:script
                      :type "text/javascript"
                      (str (ps (defvar src (new (-event-source "/sub")))
                               (defun p (msg)
                                 (let ((elem (chain document (get-element-by-id "console"))))
                                   (setf (@ elem inner-h-t-m-l)
                                         (+ (@ elem inner-h-t-m-l) "<p>" msg "</p>"))))
                               (defun addtx (txdata)
                                 (let ((row (+ "<tr><td> ..." (chain (@ txdata hash) (substring 55))
                                               "</td><td style=\"text-align:center;\">" (@ txdata "tx-in-count")
                                               " : " (@ txdata "tx-out-count")
                                               "</td><td style=\"text-align:right;\">" (@ txdata "total-sent") "</td></tr>")))
                                   (chain ($ row) (prepend-to "#txn-list"))))
                               (defun addblock (blkdata)
                                 (let ((row (+ "<tr><td>" (to-string (new (Date (@ blkdata timestamp))))
                                               "</td><td>" (@ blkdata numtx)
                                               "</td><td>" (@ blkdata diff))))
                                   (chain ($ row) (prepend-to "#blk-list"))))
                               (setf (@ src onerror)
                                     (lambda (e)
                                       (p "error:")
                                       (p (chain -j-s-o-n (stringify e))))
                                     (@ src onopen)
                                     (lambda (e) (p "Connected..."))
                                     (@ src onmessage)
                                     (lambda (e)
                                       (let ((data (chain -j-s-o-n (parse (@ e data)))))
                                         (if (= (@ data type) "tx")
                                             (addtx data)
                                             (addblock data)))))))))))))
    (cat "HTTP/1.1 200 OK" crlf
         "Content-Type: text/html; charset=UTF-8" crlf
         "Cache-Control: no-cache, no-store, must-revalidate" crlf
         "Content-Length: " (write-to-string (length content)) crlf crlf
         content crlf crlf)))
