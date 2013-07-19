;;;; pl1.lisp

(in-package #:pl1)

(defparameter pkg (symbol-package :keyword))

(defun classify-statement (tokens)
  (if (and (> (length tokens) 1)
           (eq :id (car (elt tokens 0)))
           (eq :char (car (elt tokens 1))))
      'assignment
      (case (cdr (elt tokens 0))
        ((:procedure) 'procedure)
        ((:end)       'end)
        (otherwise    'statement)
        )
      )
  )

(defun make-statement (tokens &key level nesting)
  (let ((stmt)
        (statement-type)
        )
    (iter
     (for start on tokens by #'cddr)
     (while (and (eq :id (caar start)) (equal (cons :char #\:) (cadr start))))
     (collect (cdar start) into labels)
     (finally
      (setf statement-type (classify-statement start))
      (setf stmt (make-instance statement-type :labels labels :tokens start
                                :level level :nesting nesting))
      (parse stmt)
      )
     )
    stmt
    )
  )

(defun insert-id (id hash value)
  (if (gethash id hash)
      (error "Duplicate definition for ~a" id)
      (setf (gethash id hash) value)
      )
  )
(defparameter tokeniser (create-scanner
                         "(('([^']('')?)*')|([-+]?\\d+([.]\\d*)?)|(\\w[\\w\\d_]*)|(\\s+)|(.))"
                         :multi-line-mode t :extended-mode t))
(defun tokenise (string)
  (let* ((str (regex-replace-all "\/[*][^*]*([*][^/])*[*]\/" string " "))
         (result (make-list 0))
         )
    (do-scans (match-start match-end reg-starts reg-ends tokeniser str)
      (if (null (elt reg-starts 7))
          (setf result
                (push
                 (if (elt reg-starts 1)
                     (cons :string
                           (regex-replace-all "''"
                                              (subseq str
                                                      (1+ match-start)
                                                      (1- match-end))
                                              "'"))
                     (if (elt reg-starts 4)
                         (multiple-value-bind (num complete)
                             (read-from-string (subseq str match-start
                                                       match-end)
                                               :preserve-whitespace t)
                           (unless (< complete (- match-end match-start))
                             (cons :number num)
                             )
                           )
                         (if (elt reg-starts 6)
                             (cons :id (fix-abbreviations
                                        (intern
                                         (string-upcase (subseq str
                                                                match-start
                                                                match-end))
                                         pkg)))
                             (cons :char (elt str match-start))
                             )
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

