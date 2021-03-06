;;;; classes.lisp

(in-package #:pl1)

(defparameter pkg (symbol-package :keyword))

(defvar current-block)
(defvar block-number)
(defvar current-token)

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

(defmethod update-nesting ((self assignment)) (nesting self))
(defmethod update-level ((self assignment)) (level self))

(defmethod update-nesting ((self statement)) (nesting self))
(defmethod update-level ((self statement)) (level self))

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

(defmethod parse ((self statement))
  )

(defmethod parse ((self assignment))
  (let ((current-token (tokens self))
        )
    (setf (lhs self) (var)
          )
    (if (equal '(:token . #\=) (car current-token))
        (progn
          (setf current-token (cdr current-token))
          (setf (rhs self) (expression))
          )
        (error "Invalid assignment statement ~a current-token ~a"
               (tokens self) current-token)
        )
    )
  )

(defmethod parse ((self program))
  (let ((tokens (tokenise (remove-comments (source self))))
        (current-statement (make-list 0))
        (statements (make-list 0))
        (nesting 0)
        (level 0)
        )
    (iter
      (for token in tokens)
      (if (equal token '(:token . #\;))
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

(defmethod print-object ((self assignment) stream)
  (call-next-method)
  (format stream "~a = ~a" (lhs self) (rhs self))
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

(defun var ()
  (id)
  )

(defun id ()
  (let ((curr (car current-token))
        (next (cdr current-token))
        )
    (if (eq (car curr) :id)
        (progn
          (setf current-token next)
          curr
          )
        nil
        )
    )
  )

(defun expression ()
  (or (var) (num))
  )

(defun num ()
  (let ((curr (car current-token))
        (next (cdr current-token))
        )
    (if (eq :number (car curr))
        (progn
          (setf current-token next)
          curr
          )
        nil
        )
    )
  )
