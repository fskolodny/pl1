;;;; package.lisp

(defpackage #:pl1
  (:use #:cl
        #:utils
        #:iter
        #:cl-ppcre
        )
  (:export #:children
           #:make-program
           #:symbol-table
           #:definition
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

