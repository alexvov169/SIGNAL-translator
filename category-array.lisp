(in-package :cl-user) 

(uiop:define-package :signal-translator/category-array
    (:nicknames :categorizer)
  (:use :common-lisp)
  (:export #:with-attributes-table
           #:range))

(in-package :signal-translator/category-array)

(defun attribute-clause-to-setfs (attribute-clause array)
  (let ((condition (first attribute-clause))
        (attribute `(progn ,@(rest attribute-clause))))
    (cond ((atom condition)
           (cond ((numberp condition)
                  `(setf (aref ,array ,condition)
                         ,attribute))
                 ((eq t condition))
                 (t '(error "wrong type of atom condition"))))
          ((symbolp (first condition))
           (case (first condition)
             (or
              `(progn ,@(mapcar (lambda (or-condition)
                                  (attribute-clause-to-setfs (cons or-condition
                                                                   (rest attribute-clause))
                                                             array))
                                (rest condition))))
             (range
              (cond ((and (numberp (second condition))
                          (numberp (third condition)))
                     (let ((i-sym (gensym)))
                       `(do ((,i-sym ,(second condition) (1+ ,i-sym)))
                            ((>= ,i-sym ,(third condition)))
                          (setf (aref ,array ,i-sym)
                                ,attribute))))
                    (t (error "wrong type of range arguments"))))))
          (t (error "wrong condition")))))
                  
(defmacro with-attributes-table (name dimension default attribute-clauses &body body)
  "`attribute-clauses' are to match following grammar:
`attribute-clauses' ::= `attribute-clause' *
`attribute-clause' ::= '(' `condition' `attribute' ')'
`condition' ::= `number' | '(' 'or `conditions' ')' | '(' 'range `number' `number' ')'
`conditions' ::= `condition' *"
  `(let ((,name (make-array ,dimension :initial-element ,default)))
     ,@(mapcar (lambda (attribute-clause)
                 (attribute-clause-to-setfs attribute-clause
                                            name))
               attribute-clauses)
     ,@body))
