;;;; ftp.lisp

(in-package #:ftp)

(defmacro with-socket-stream ((name socket-conn) &body body)
  `(let ((,name (socket-stream ,socket-conn)))
     ,@body))

(defun init-session ()
  (-> (empty-map)
      (with 'user nil)
      (with 'auth nil)
      (with 'status 0)
      (with 'root-dir "~/")
      (with 'cwd "~/")))

;; Bare minimum I need to write to call this FTP:
;; USER QUIT PORT TYPE MODE STRU RETR STOR NOOP
;; USER user      Login step 1
;; PASS pass      Login step 2
;; QUIT           Logout if not transferring
;; PORT port      Change data transfer port (just use decimal)
;; STRU F/R/P     Set structure to (F)ile, (R)ecord, or (P)age
;;                (Just return 500 and keep set to File)
;; MODE S/B/C     Transfer in (S)tream, (B)lock, or (C)ompressed
;;                TCP good enough to decide first 2, but compression
;;                could be worth considering
;; RETR path      Server serves file at path
;; STOR path      Server stores file at path, overwrites
;; RNFR path      Rename from path, must be followed by RNTO
;; RNTO path      Rename to path
;; DELE path      Delete at path. No questions asked.
(defun authenticate (session pass)
  (if (and (equal (@ session 'user) "k")
           (equal pass "k"))
      (values (with session 'auth t) "230")
      (values session "530")))

(defun user (session user)
  (let ((users '("k" "admin")))
    (if (member user users :test #'equal)
        (values (with session 'user user) "230")
        (values session "530"))))

(defun quit (session)
  (values (with session 'auth nil) "200"))

(defun auth? (session)
  (@ session 'auth))

(defmacro with-auth (session &body body)
  `(if (auth? ,session)
       ,@body
       (values session "530")))

(defun no-op (session)
  (with-auth session
    (values session "500")))

(defun ret-code (code string)
  (concatenate 'string code " " string))

(defun pwd (session)
  (with-auth session
    (let ((ret (@ session 'dir)))
      (values session (ret-code "200" ret)))))
  
(defun handle-cmd (cmd session)
  (match (split " " cmd)
    ((list "user" user)
     (user session user))
    ((list "pass" pass)
     (authenticate session pass))
    ((list "pwd")
     (pwd session))
    (_ (no-op session))))

(defun cmd-loop (session)
  (let ((cmd (string-downcase (read-line))))
    (multiple-value-bind (session code)
        (handle-cmd cmd session)
      (format t "SENDING CODE: ~A~%" code)
      (format t "SESSION STATE: ~A~%" session)
      (if (equal cmd "quit")
          (progn
            (format t "QUITTING")
            session)
          (progn
            (format t "Entered command: ~A~%" cmd)
            (cmd-loop session))))))
           
    
(defun recv-cmd (client)
  (format t "Awaiting CMD from ~A~%" (get-peer-name client))
  (with-socket-stream (conn client)
    (wait-for-input client)
    (let ((cmd (read-line conn nil :QUIT)))
      (format t "Cmd from ~a: ~a" client cmd)
      cmd)))


;; I would LIKE to run this on two threads, but that isn't happenning.
;; Shouldn't be a problem anyways. The TRX process SHOULDN'T run without
;; clearance from the control process anyways.
;; Probably for the best that we implement it like that.
(defun handle-client (client-conn)
  (format t "Handling ~a~%" client-conn)
  (unwind-protect
       (let ((cmd (recv-cmd client-conn)))
         (format t "Toplevel again. CMD: ~A~%" cmd))
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


(defun byte-socket (port)
  (let ((socket (socket-listen "127.0.0.1" port :reuse-address t)))
    (format t "Listenting for incoming connections~%")
    (force-output)
    (unwind-protect
         (loop
           do (let* ((conn (socket-accept socket
                                          :element-type '(unsigned-byte 8)))
                     (peer (get-peer-name conn)))
                (format t "Connection from ~A, transferring~%" peer)
                (force-output)
                (transfer-bytes "bytes.txt" conn)
                (format t "Transfer to ~A complete, closing.~%" peer)
                (force-output)
                (socket-close conn)))
      (progn
        (format t "Closing data transfer server~%")
        (socket-close socket)))))
      


(defun transfer-bytes (file socket)
  "Transfer contents of FILE to receiving SOCKET"
  (let ((output-stream (socket-stream socket)))
    (with-open-file (fh file :direction :input
                             :element-type '(unsigned-byte 8))
      (buffer-byte-streams fh output-stream)
      (socket-close socket))))


"
CDUP - Change to Parent Directory
SMNT - Structure Mount
STOU - Store Unique
RMD - Remove Directory
MKD - Make Directory
PWD - Print Directory
SYST - System

Connection akin to TELNET


USER NAME user - ID by username
PASSWORD pass - ID by password
ACCOUNT acct 
"

