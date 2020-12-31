;;;; ftp.asd

(asdf:defsystem #:ftp
  :description "Describe ftp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:usocket #:fset #:lparallel #:arrows)
  :components ((:file "package")
               (:file "ftp")))
