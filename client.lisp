

(in-package :ftp)

(defun client (port &optional (buffer-size 10))
  (let* ((socket (socket-connect "127.0.0.1" port
                                 :element-type '(unsigned-byte 8)))
         (recv-stream (socket-stream socket)))
    
    
    (unwind-protect
         (with-open-file (fh "output.txt" :direction :output
                                          :element-type '(unsigned-byte 8)
                                          :if-does-not-exist :create
                                          :if-exists :overwrite)
           (buffer-byte-streams recv-stream fh buffer-size))
      (socket-close socket))))

