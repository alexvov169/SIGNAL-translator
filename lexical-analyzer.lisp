(in-package :cl-user) 

(uiop:define-package :signal-translator/lexical-analyzer
    (:nicknames :lexer)
  (:use :common-lisp :signal-translator/category-array
        )
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

(defun make-keyword-table (keywords)
  (let ((result (make-hash-table :test #'equal))
        (id 257))
    (mapc (lambda (keyword)
            (setf (gethash keyword result)
                  id)
            (incf id))
          keywords)
    result))

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


(defun lexical-analyzer (filespec identifier-first constant-first
                         keywords-table
                         error-handler
                         &optional
                           (identifiers-table (make-hash-table))
                           (constants-table (make-hash-table :test #'equalp)))
  "Handles following character categories: 
`%single-char-token', 
`%or-keyword-identifier',
`%undefined'"
  (with-open-signal-program filespec
    (let ((token-position nil)
          (constant-offset 0)
          (identifier-offset 0))
      (labels
          ((%out (token)
             (vector-push-extend (list token-position token)
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
             (%out (char-code curr-char))
             (%inp))

           (%identifier ()
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
           
           
           (%or-keyword-identifier ()
             (%out
              (let ((string (%identifier)))
                (or (gethash string keywords-table)
                    (or (gethash string identifiers-table)
                        (let ((identifier-hash
                                (+ identifier-first identifier-offset)))
                          (setf (gethash string
                                         identifiers-table)
                                identifier-hash)
                          (incf identifier-offset)
                          identifier-hash))))))
           
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
               (let ((constant-hash (+ constant-first constant-offset)))
                 (setf (gethash constant-hash constants-table)
                       (list re im))
                 (incf constant-first)
                 (%out constant-hash))))

           (%com-eof-p (com)
             (when (eq 'eof com)
               (%unexpected-eof-err)
               t))

           (%unexpected-eof-err ()
             (%err-str nil :eof :before "end of comment"))

           (%inp () (setf curr-char
                          (funcall scanner)))
           
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
                              ((or (range 48 57)
                                   36 41) #'%unexpected)
                              ((range 65 90) #'%or-keyword-identifier)
                              ((or 46 59 61) #'%single-char-token))
                    
                    (funcall (aref ascii-handler-array
                                   (char-code char))))
                  )))
          
          (do ()
              ((eq 'eof curr-char)
               (list token-table
                     identifiers-table
                     constants-table))
            (setf token-position (funcall positioner))
            (funcall get-char-handler
                     curr-char)))))))

(defun lexer (filespec identifier-first constants-first)
  (destructuring-bind (error-handler error-table-getter) (error-keeper)
    (let ((keywords-table
            (make-keyword-table (list "PROGRAM"
                                      "BEGIN"
                                      "END"
                                      "CONST"))))
      (values (lexical-analyzer  filespec
                                 identifier-first
                                 constants-first
                                 keywords-table
                                 error-handler)
              keywords-table
              (funcall error-table-getter)))))
