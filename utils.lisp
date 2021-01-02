;;;; utils.lisp
;;;; Needful functions, and things shared between client and server code

(in-package :ftp)

(defun buffer-byte-streams (read-stream write-stream
                            &optional (buffer-size 10))
  "Send bytes from READ-STREAM to WRITE-STREAM in BUFFER-SIZE blocks"
  (let ((buffer (make-array buffer-size :initial-element nil)))
    (loop for bytes-read = (read-sequence buffer read-stream)
          while (< 0 bytes-read)
          do (progn
               (write-sequence (subseq buffer 0 bytes-read)
                               write-stream)
               (force-output write-stream)))))
