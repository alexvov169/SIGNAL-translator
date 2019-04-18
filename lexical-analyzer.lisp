(in-package :cl-user) 

(uiop:define-package :signal-translator/lexical-analyzer
    (:nicknames :lexer)
  (:use :common-lisp :signal-translator/category-array)
  (:export #:lexer
           #:eof
           #:digit))

(in-package :lexer)

(defun error-keeper ()
  (let ((error-table (make-array 0 :adjustable t
                                   :fill-pointer 0)))
    (list (lambda (message)
            (vector-push-extend message error-table))
          (lambda () error-table))))

(defun setvkkv-table (key value kvtable vktable)
  (setf (gethash key kvtable) value)
  (setf (gethash value vktable) key))

(defun make-identifiers-tables-wrapper ()
  (let ((num+id-table (make-hash-table :test #'eq))
	(id+num-table (make-hash-table :test #'equal))
	(num 0))
    (list (lambda (id) (gethash id id+num-table))
	  (lambda (id)
	    (setvkkv-table num id num+id-table id+num-table)
	    (prog1 num
	      (incf num)))
	  (lambda () (list num+id-table id+num-table)))))

(defun make-keyword-table (keywords)
  (destructuring-bind (get-key set-id get-tabs) (make-identifiers-tables-wrapper)
    (declare (ignore get-key))
    (mapc set-id keywords)
    (funcall get-tabs)))

(defmacro with-open-signal-program (filespec
                                    &body body)
  `(with-open-file (stream ,filespec)
     (with-attributes-table category-array 128
         'err (((or 32 8 9 10 13) 'ws)
	       (39 'constant)
               (40 'or-comment-error)
               ((range 48 57) 'digit)
               ((range 65 90) 'letter)
               ((or 46 59 61) 'single-char-delimiter))
       (let* ((char-position 0)
              (line-position 1)
              (scanner (lambda ()
                         (let ((result (read-char stream nil 'eof)))
                           (cond ((eq 'eof result) 'eof)
                                 ((eq #\NewLine result)
                                  (incf line-position)
                                  (setf char-position 0)
                                  result)
                                 (t
                                  (incf char-position)
                                  result)))))
              (positioner (lambda ()
                            (list line-position
                                  char-position)))
              (token-table (make-array 0 :adjustable t
                                         :fill-pointer 0))
              (curr-char (funcall scanner))
              (char-categorizer (lambda (char)
                                  (aref category-array
                                        (char-code char)))))
         ,@body))))


(defun lexical-analyzer (filespec keywords-table id-wrapper error-handler
                         &optional
                           (constants-table (make-hash-table :test #'equalp)))
  "Handles following character categories: 
`%single-char-token', 
`%or-keyword-identifier',
`%undefined'"
  (destructuring-bind (get-key set-id get-tabs) id-wrapper
    (with-open-signal-program filespec
      (let ((token-position nil)
	    (constant-offset 0))
	(labels
	    ((%out (token-start-sym token)
	       (vector-push-extend (list token-position (list token-start-sym token))
				   token-table))
	     (%ws ()
	       (do ((ws (%inp) (%inp)))
		   ((or (eq 'eof ws)
			(not (%predicate 'ws ws))))))
	     
	     (%err-str (expected-p un/expected-what
			&key
			  (to nil to-p)
			  (before nil before-p)
			  (after nil after-p))
	       (%error (format nil "~aexpected ~s ~a~a~a~a~a"
			       (if expected-p "" "not ")
			       un/expected-what
			       (if after-p
				   (format nil "after ~s, " after)
				   "")
			       (if before-p
				   (format nil "before ~a, " before)
				   "")
			       (if to-p
				   (format nil "to ~a, " to)
				   "")
			       (if expected-p 
				   (format nil "got ~s " curr-char)
				   "")
			       (let ((position (funcall positioner)))
				 (format nil "at line ~a, column ~a"
					 (first position)
					 (second position))))))

	     (%error (message)
	       (funcall error-handler message))

	     (%single-char-token ()
	       (%out :single-char (char-code curr-char))
	       (%inp))

	     (%string ()
	       (let ((result (make-array 0 :adjustable t
					   :fill-pointer 0)))
		 (vector-push-extend curr-char result)
		 (do ((current (%inp) (%inp)))
		     ((or (eq current 'eof)
			  (not (or (eq (funcall char-categorizer current)
				       'letter)
				   (eq (funcall char-categorizer current)
				       'digit))))
		      (coerce result 'string))
		   (vector-push-extend current result))))

	     (%digit-to-num (char)
	       (- (char-code char)
		  (char-code #\0)))
	     
	     (%unsigned ()
	       (do ((current curr-char (%inp))
		    (result 0 (+ (%digit-to-num current)
				 (* 10 result))))
		   ((or (eq current 'eof)
			(not (eq (funcall char-categorizer current)
				 'digit)))
		    result)))

	     (%identifier (string)
	       (let ((id-hash (or (funcall get-key string)
				  (funcall set-id string))))
		 (%out :identifier id-hash)
		 id-hash))

	     (%keyword (string)
	       (let ((kw-hash (gethash string keywords-table)))
		 (when kw-hash (%out :keyword kw-hash) kw-hash)))
	     
	     (%or-keyword-identifier ()
	       (let ((string (%string)))
		 (or (%keyword string)
		     (%identifier string))))
	     
	     (%predicate (attribute char)
	       (eq attribute (funcall char-categorizer char)))
	     
	     (%constant ()
	       (let* ((new-char (%inp))
		      (re (list 're
				(cond ((eq 'eof new-char)
				       (%err-str nil "EOF" :after "#\\'")
				       0)
				      ((%predicate 'digit new-char)
				       (%unsigned))
				      (t 0))))
		      (im   
			(cond ((eq #\, curr-char)
			       (%inp)
			       (list
				'im
				(cond ((%predicate 'digit curr-char)
				       (%unsigned))
				      (t (%err-str t "digit"
						   :to "start imaginary part")
					 0))))
			      ((and (eq #\$ curr-char)
				    (eq #\E (%inp))
				    (eq #\X (%inp))
				    (eq #\P (%inp))
				    (eq #\( (%inp)))
			       (%inp)
			       (list
				'exp
				(cond ((%predicate 'digit curr-char)
				       (prog1 (%unsigned)
					 (cond ((eq #\) curr-char) (%inp))
					       (t (%err-str t #\)
							    :to "finish imaginary exponent part")))))
				      (t (%err-str t :digit
						   :to "start imaginary exponent part")
					 0))))
			      (t 0))))
		 (case curr-char
		   (#\' (%inp))
		   (t (%err-str t #\' :to "finish constant")))
		 (let ((constant-hash constant-offset))
		   (setf (gethash constant-hash constants-table)
			 (list re im))
		   (incf constant-offset)
		   (%out :constant constant-hash))))

	     (%com-eof-p (com)
	       (when (eq 'eof com)
		 (%unexpected-eof-err)
		 t))

	     (%unexpected-eof-err ()
	       (%err-str nil :eof :before "end of comment"))

	     (%inp () (setf curr-char (funcall scanner)))
	     
	     (%comment ()
	       (do ()
		   ((and (do ((com (%inp) (%inp)))
			     ((or (and (%com-eof-p com)
				       (return-from %comment))
				  (eq #\* com))
			      t))
			 (eq #\)
			     (do ((ecom (%inp) (%inp)))
				 ((or (and (%com-eof-p ecom)
					   (return-from %comment))
				      (not (eq #\* ecom)))
				  ecom))))))
	       (%inp))
	     
	     (%or-comment-error ()
	       (case (%inp)
		 (#\* (%comment))
		 (eof (%unexpected-eof-err))
		 (t
		  (%err-str t #\* :after #\( :to "open a comment"))))
	     
	     (%err ()
	       (%err-str nil curr-char :to "be here, it\'s illegal character")
	       (%inp))
	     (%unexpected ()
	       (%err-str nil curr-char :to "be here")
	       (%inp)))

	  (let ((get-char-handler
		  (lambda (char)
		    (with-attributes-table ascii-handler-array 128
			#'%err (((or 32 8 9 10 13) #'%ws)
				(39 #'%constant)
				(40 #'%or-comment-error)
				((or (range 48 57) 36 41) #'%unexpected)
				((range 65 90) #'%or-keyword-identifier)
				((or 46 59 61) #'%single-char-token))
		      
		      (funcall (aref ascii-handler-array
				     (char-code char))))
		    )))
	    
	    (do ()
		((eq 'eof curr-char)
		 (list token-table
		       (funcall get-tabs)
		       constants-table))
	      (setf token-position (funcall positioner))
	      (funcall get-char-handler
		       curr-char))))))))

(defun lexer (filespec)
  (destructuring-bind (id+keyword-table keyword+id-table)
      (make-keyword-table (list "PROGRAM"
				"BEGIN"
				"END"
				"CONST"))
    (destructuring-bind (error-handler error-table-getter)
	(error-keeper)
      (list (lexical-analyzer filespec
			      keyword+id-table
			      (make-identifiers-tables-wrapper)
			      error-handler)
	    (list id+keyword-table keyword+id-table)
	    (funcall error-table-getter)))))
