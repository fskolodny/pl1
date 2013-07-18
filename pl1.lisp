;;;; pl1.lisp

(in-package #:pl1)

(defparameter pkg (symbol-package :keyword))

(defclass blk ()
  ((level :accessor nesting :initarg level)
   (children :accessor children :initform (make-list 0))
   (symtab :accessor symbol-table :initform (make-hash-table :test 'equalp))
   )
  )

(defclass procedure (blk)
  (
   )
  )

(defclass begin (blk)
  (
   )
  )

(defclass program ()
  ((source :accessor source :initarg :source :type 'string)
   (name-table :accessor external-name-table
               :initform (make-hash-table :test 'equalp))
   (procs :accessor external-procedures :initform (make-list 0))
   (statements :accessor statements)
   )
  )

(defclass statement ()
  ((lbls :accessor statement-labels :initarg :labels)
   (tokens :accessor tokens :initarg :tokens)
   (nesting :accessor nesting)
   (blk :accessor block-of)
   (level :accessor level)
   )
  )

(defun make-statement (tokens)
  (let ((stmt)
        )
    (iter
     (for start on tokens by #'cddr)
     (while (and (eq :id (caar start)) (equal (cons :char #\:) (cadr start))))
     (collect (cdar start) into labels)
     (finally
      (setf stmt (make-instance 'statement :labels labels :tokens start))
      )
     )
    stmt
    )
  )

(defun tokenise (string)
  (let* ((str (regex-replace-all "\/[*][^*]*([*][^/])*[*]\/" string " "))
         (tokeniser (create-scanner
                     "(('([^']('')?)*')|(\\w[\\w\\d_]*)|(\\s+)|(.))"
                     :multi-line-mode t :extended-mode t))
         (result (make-list 0))
         )
    (do-scans (match-start match-end reg-starts reg-ends tokeniser str)
      (if (null (elt reg-starts 5))
          (setf result (push
                        (if (elt reg-starts 1)
                            (cons :string
                                  (regex-replace-all "''"
                                                     (subseq str
                                                             (1+ match-start)
                                                             (1- match-end))
                                                     "'"))
                            (if (elt reg-starts 4)
                                (cons :id (fix-abbreviations
                                           (intern
                                            (string-upcase (subseq str
                                                                   match-start
                                                                   match-end))
                                            pkg)))
                                (cons :char (elt str match-start))
                                )
                            )
                        result)
                )
            )
      )
    (reverse result)
    )
  )

(defun fix-abbreviations (sym)
  (case sym
    ((:proc) :procedure)
    ((:dcl) :declare)
    (otherwise sym)
    )
  )

(defmethod parse ((self program))
  (let ((tokens (tokenise (source self)))
        (current-statement (make-list 0))
        (statements (make-list 0))
        )
    (iter
      (for token in tokens)
      (if (and (eq (car token) :char) (char= #\; (cdr token)))
          (progn
            (setf statements (push (make-statement (reverse current-statement))
                                   statements)
                  current-statement (make-list 0)
                  )
            )
          (setf current-statement (push token current-statement))
          )
      )
    (setf (statements self) (reverse statements))
    )
  self
  )

(defmethod print-object ((self statement) stream)
  (format stream "~%~{~a:~^ ~}~{~a~^ ~};"
          (statement-labels self) (mapcar #'cdr (tokens self))
          )
  )

(defmethod print-object ((self program) stream)
  (format stream "~%~s~%~{~s~}" (source self) (statements self))
  )

(defmethod make-program ((source string))
  (let ((prog (make-instance 'program :source source))
        )
    (parse prog)
    )
  )
