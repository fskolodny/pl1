;;;; pl1.asd

(asdf:defsystem #:pl1
  :serial t
  :description "Describe pl1 here"
  :author "Fila Kolodny <fskolodny@gmail.com>"
  :license "Specify license here"
  :depends-on (
               #:utils
	       )
  :components ((:file "package")
               (:file "pl1")))

