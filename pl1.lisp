;;;; pl1.lisp

(in-package #:pl1)

(defparameter pkg (symbol-package :keyword))

(defun assignmentp (tokens)
  (and (> (length tokens) 2)
       (equal '(:token . #\=) (elt tokens 1))
       )
  )

(defun classify-statement (tokens)
  (if (assignmentp tokens)
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
     (while (and (eq :id (caar start)) (equal '(:token . #\:) (cadr start))))
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
                         "(('([^']('')?)*')|([-+]?\\d+([.]\\d*)?)|(\\w[\\w\\d_]*)|(\\s+)|((!=)|(>=)|(<=)|(->)|.))"
                         :multi-line-mode t :extended-mode t))
(defparameter comments-re (create-scanner "\/[*][^*]*([*][^/])*[*]\/"))

(defun remove-comments (string)
  (regex-replace-all comments-re string " ")
  )
(defun tokenise (string)
  (let* ((result (make-list 0))
         )
    (do-scans (match-start match-end reg-starts reg-ends tokeniser string)
      (if (null (elt reg-starts 7))
          (setf result
                (push
                 (if (elt reg-starts 1)
                     (cons :string
                           (regex-replace-all "''"
                                              (subseq string
                                                      (1+ match-start)
                                                      (1- match-end))
                                              "'"))
                     (if (elt reg-starts 4)
                         (multiple-value-bind (num complete)
                             (read-from-string (subseq string match-start
                                                       match-end)
                                               :preserve-whitespace t)
                           (unless (< complete (- match-end match-start))
                             (cons :number num)
                             )
                           )
                         (if (elt reg-starts 6)
                             (cons :id (fix-abbreviations
                                        (intern
                                         (string-upcase (subseq string
                                                                match-start
                                                                match-end))
                                         pkg)))
                             (cons :token
                                   (if (= 1 (- match-end match-start))
                                       (elt string match-start)
                                       (intern (subseq string match-start
                                                       match-end)
                                               pkg)))
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

