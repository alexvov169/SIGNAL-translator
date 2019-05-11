(in-package :cl-user) 

(uiop:define-package :signal-translator/markup-code
    (:nicknames :markup-code)
  (:use :common-lisp :cl-markup)
  (:export #:markup-code
	   #:markup-head
	   #:markup-table
	   #:markup-data-rows))

(in-package :markup-code)

(defun markup-head (head-row)
  (cons :tr
	(mapcar (lambda (h) (list :th h))
		head-row)))

(defun markup-row (row)
  (cons :tr
	(mapcar (lambda (d) (list :td d))
		row)))

(defun markup-data-rows (data-rows)
  (mapcar #'markup-row data-rows))

(defun markup-table (head-row data-rows)
  (cons :table
	(cons (markup-head head-row)
	      (markup-data-rows data-rows))))

(defun create-numbered-code (lines)
  (let ((line-num 0))
    (mapcar (lambda (line) (list* (incf line-num) #\  line))
	    (mapcar (lambda (line) (coerce line 'list))
		    lines))))

(defun markup-code (lines)
  (markup-table nil (create-numbered-code lines)))
