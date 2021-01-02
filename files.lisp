;; Main utilities for working with files

(in-package :ftp)


(defvar *base* (uiop:native-namestring "~/sandbox/lisp/ftp/accts"))

(defun user-repr (path)
  (concatenate 'string "~" (subseq path 31)))

   
(defun dir-namestring (dir)
  (concatenate 'string *base* dir))
  
(defun user-dir (user)
  (dir-namestring user))
  
(defun init-dirs ()
  (let ((file (uiop:native-namestring "~/sandbox/lisp/ftp/accts/")))
    (ensure-directories-exist file)))
;; ~/wherever should always actually refer to /user/wherever

