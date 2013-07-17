;;;; package.lisp

(defpackage #:pl1
  (:use #:cl
        #:utils
        #:iter
        )
  (:export #:external-procedures
           #:make-program
           #:external-name-table
           #:source
           )
  )
(defpackage #:pl1.tests
  (:use #:cl
        #:pl1
        #:fiveam
        )
  )

