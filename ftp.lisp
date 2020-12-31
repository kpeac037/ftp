;;;; ftp.lisp

(in-package #:ftp)

(defun set-test ()
  (let ((i (empty-set)))
    (-> i
        (with 3)
        (with 4)
        (with 5))))




(defun handle-client (client-conn)
  (format t "Handling ~a~%" client-conn)
  (unwind-protect
       (let ((client (socket-stream client-conn)))
         (format client "HELLO\n")
         (force-output client)
         (wait-for-input client-conn)
         (format t "Input from ~A: ~A~%" client-conn (read-line client))) 
    (progn
      (format t "Closing connection to ~a~%" client-conn)
      (socket-close client-conn))))
    
              
(defun start-server (port)
  (let ((socket (socket-listen "127.0.0.1" port :reuse-address t)))
    (unwind-protect
         (loop
            (let ((conn (socket-accept socket
                                       :element-type 'character)))
              (handle-client conn)))
      (progn
        (format t "Closing server")
        (socket-close socket)))))
        

"
CDUP - Change to Parent Directory
SMNT - Structure Mount
STOU - Store Unique
RMD - Remove Directory
MKD - Make Directory
PWD - Print Directory
SYST - System

Connection akin to TELNET
"

