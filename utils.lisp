(in-package :cl-user) 

(uiop:define-package :signal-translator/utils
    (:nicknames :utils)
  (:use :common-lisp :alexandria)
  (:export #:aif
	   #:ignore-if-in-error
	   #:ignore-if-in-error-else-list
	   #:*in-error*))

(in-package :utils)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aif-error (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if (eq it :error) ,then-form ,else-form)))

(defmacro a-unless-error (test-form &body else-form)
  `(aif-error ,test-form
	      (list :error)
	      (cons it (progn ,@else-form))))

(defmacro a-unless-error-cons (test-form &body else-form)
  `(a-unless-error ,test-form
     (cons ))
  `(aif-error ,test-form
	      (list :error)
	      (cons it (progn ,@else-form))))

(defmacro ignore-if-in-error (&body body)
  `(if *in-error* (list :ignore) (progn ,@body)))

(defmacro ignore-if-in-error-else-list (&body body)
  `(ignore-if-in-error (list (progn ,@body))))
