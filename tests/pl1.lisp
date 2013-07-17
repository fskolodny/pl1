(in-package :pl1.tests)
(defparameter empty-string "")
(defparameter simple-prog "p:proc;end;")
(fiveam:def-test main ()
  (let ((program)
        )
    (setf program (make-program empty-string))
    (is (= 0 (length (external-procedures program))))
    (is (equalp empty-string (source program)))
    (setf program (make-program simple-prog))
    (is (equalp simple-prog (source program)))
    (let ((procs (external-procedures program)))
      (is (= 1 (length procs)))
      (is-true (gethash :p (external-name-table program)))
      )
    )
  )