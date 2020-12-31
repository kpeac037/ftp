;;;; package.lisp

(defpackage #:ftp
  (:use #:cl #:fset #:arrows #:usocket)
  (:shadowing-import-from :cl
                          :first
                          :position-if
                          :substitute
                          :intersection
                          :map
                          :remove-if
                          :last
                          :remove
                          :sort
                          :find
                          :remove-if-not
                          :reverse
                          :find-if-not
                          :stable-sort
                          :position
                          :count-if-not
                          :set-difference
                          :union
                          :subseq
                          :position-if-not
                          :set
                          :find-if
                          :substitute-if
                          :substitute-if-not
                          :reduce
                          :count
                          :complement
                          :count-if))
