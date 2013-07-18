;;;; pl1.asd

(asdf:defsystem #:pl1
  :serial t
  :description "Describe pl1 here"
  :author "Fila Kolodny <fskolodny@gmail.com>"
  :license "Specify license here"
  :depends-on (
               #:utils
               #:iterate
               #:cl-ppcre
               #:fiveam
	       )
  :components ((:file "package")
               (:file "classes")
               (:file "pl1")
               (:module "tests"
                        :components (
                                     (:file "pl1")
                                     )
                        )
               )
  )

