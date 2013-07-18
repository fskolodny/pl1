;;;; pl1.lisp

(in-package #:pl1)

(defparameter pkg (symbol-package :keyword))

(defvar current-block)
(defvar block-number)

(defclass blk ()
  ((level :accessor nesting :initarg :level)
   (children :accessor children :initform (make-list 0))
   (symtab :accessor symbol-table :initform (make-hash-table :test 'eq))
   (parent :accessor parent)
   (definition :accessor definition :initform nil :initarg :definition)
   (num :accessor block-number :initarg :number)
   )
  )

(defclass procedure-block (blk)
  (
   )
  )

(defclass begin-block (blk)
  (
   )
  )

(defclass program (blk)
  ((source :accessor source :initarg :source :type 'string)
   (statements :accessor statements)
   )
  )

(defclass statement ()
  ((lbls :accessor statement-labels :initarg :labels)
   (tokens :accessor tokens :initarg :tokens)
   (nesting :accessor nesting :initarg :nesting)
   (blk :accessor block-of)
   (level :accessor level :initarg :level)
   )
  )

(defclass assignment (statement)
  ((lhs :accessor lhs)
   (rhs :accessor rhs)
   )
  )

(defclass end (statement)
  (
   )
  )

(defclass procedure (statement)
  (
   )
  )

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
      )
     )
    stmt
    )
  )

(defun insert-id (id hash value)
  (if (gethash id hash)
      (error 'duplicate-key)
      (setf (gethash id hash) value)
      )
  )

(defmethod initialize-instance ((self statement) &key)
  (call-next-method)
  (setf (block-of self) current-block)
  (iter
    (for label in (statement-labels self))
    (insert-id label (symbol-table current-block) self)
    )
  )

(defmethod initialize-instance ((self procedure) &key)
  (call-next-method)
  (incf block-number)
  (let ((proc (make-instance 'procedure-block :level (1+ (level self))
                             :definition self :number block-number))
        )
    (setf (children current-block) (push proc (children current-block)))
    (setf (parent proc) current-block)
    (setf current-block proc)
    )
  )

(defmethod initialize-instance ((self end) &key)
  (call-next-method)
  (if (= 0 (nesting self))
      (setf current-block (parent current-block)))
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

(defmethod update-nesting ((self assignment)) (nesting self))
(defmethod update-level ((self assignment)) (level self))

(defmethod update-nesting ((self end))
  (let ((nesting (nesting self))
        )
    (if (> nesting 0) (1- nesting) nesting)
    )
  )
(defmethod update-level ((self end))
  (let ((level (level self))
        (nesting (nesting self))
        )
    (if (= (nesting self) 0) (1- level) level)
    )
  )

(defmethod update-level ((self procedure)) (1+ (level self))
  )
(defmethod update-nesting ((self procedure)) (nesting self))

(defmethod parse ((self program))
  (let ((tokens (tokenise (source self)))
        (current-statement (make-list 0))
        (statements (make-list 0))
        (nesting 0)
        (level 0)
        )
    (iter
      (for token in tokens)
      (if (and (eq (car token) :char) (char= #\; (cdr token)))
          (let ((stmt (make-statement (reverse current-statement) :level level :nesting nesting))
                )
            (setf statements (push stmt statements)
                  current-statement (make-list 0)
                  )
            (setf nesting (update-nesting stmt)
                  level (update-level stmt)
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
  (format stream "~%~a~T~a~T~{~a:~^ ~}~{~a~^ ~};" (level self) (nesting self)
          (statement-labels self) (mapcar #'cdr (tokens self))
          )
  )

(defmethod print-object ((self program) stream)
  (format stream "~%~s~%~{~s~}" (source self) (statements self))
  )

(defmethod make-program ((source string))
  (let* ((block-number 0)
         (prog (make-instance 'program :source source :number block-number))
         (current-block prog)
         )
    (parse prog)
    )
  )
