;;;; package.lisp

(defpackage #:pl1
  (:use #:cl
        #:utils
        #:iter
        #:cl-ppcre
        )
  (:export #:external-procedures
           #:make-program
           #:external-name-table
           #:source
           #:statements
           )
  )
(defpackage #:pl1.tests
  (:use #:cl
        #:pl1
        #:fiveam
        )
  )

