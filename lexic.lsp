(defstruct lexical-result
  (delimiters nil :type hash-table)
  (mult-delimiters nil :type hash-table)
  (key-words nil :type hash-table)
  (constants nil :type hash-table)
  (identifiers nil :type hash-table))

(defstruct tables
  (delimiters (make-hash-table :test 'equal) :type (or nil hash-table))
  (mult-delimiters (make-hash-table :test 'equal) :type (or nil hash-table))
  (key-words (make-hash-table :test 'equal) :type (or nil hash-table))
  (constants (make-hash-table :test 'equal) :type (or nil hash-table))
  (identifiers (make-hash-table :test 'equal) :type (or nil hash-table)))

(defparameter *path-to-info* "~/study/-LISP-/")

(defmethod print-object ((object hash-table) stream)
  (format stream "#<~A :TEST ~A :COUNT ~A>~%"
	  (type-of object) (hash-table-test object) (hash-table-count object))
  (maphash (lambda (k v)
	     (format stream "~4T(~S -> ~A)~%" k v))
	   object))

(defun load-table-from-file (stream start-id &optional end-id char)
  (let ((hash (make-hash-table :test `equal)))
    (labels ((%load-table (id)
	       (let ((line (read-line stream nil)))
		 (when line
		   (if (or (null end-id) (<= id end-id))
		       (setf (gethash (if char (char line 0) (string-upcase line)) hash) id)
		       (error "Id ~A is out of range ~A" id (list start-id end-id)))
		   (%load-table (1+ id))))))
      (%load-table start-id)
      hash)))

(defclass table-mixin ()
  ((table :initarg :table
	  :initform (make-hash-table :test 'equal)
	  :reader table)))

(defclass id-mixin ()
  ((current-id :initarg :current-id
	       :initform 501
	       :reader get-id
	       :writer set-id)))

(defclass const-table (table-mixin id-mixin)
  ((max-id :initarg :max-id
	   :initform 1000
	   :reader max-id)))

(defclass ids-table (table-mixin id-mixin)
  ())

(defgeneric add-element (obj value))

(defmethod add-element ((obj const-table) value)
  (let* ((id (get-id obj))
	(table (table obj))
	(res (gethash value table)))
    (if res
	res
	(if (< id (max-id obj))
	    (progn
	      (set-id (1+ id) obj)
	      (setf (gethash value (table obj)) id))
	    (error "The table is owercrowded!~%")))))

(defmethod add-element ((obj ids-table) value)
  (let* ((table (table obj))
	 (res (gethash value table)))
    (if res
	res
	(progn (setf (gethash value table) (get-id obj))
	       (set-id (1+ (get-id obj)) obj)))))

(defun load-tables (path)
  (make-tables
   :delimiters
   (with-open-file (stream (concatenate 'string path "delimiters.info"))
     (load-table-from-file stream 0 255 T))
   :mult-delimiters
   (with-open-file (stream (concatenate 'string path "mult-delimiters.info"))
     (load-table-from-file stream 301 400))
   :key-words
   (with-open-file (stream (concatenate 'string path "keywords.info"))
     (load-table-from-file stream 401 500))))

(defparameter *delimiters* nil)
(defparameter *mult-delimiters* nil)
(defparameter *key-words* nil)
(defparameter *constants* nil)
(defparameter *identifiers* nil)
(defparameter *errors* nil)

(defmacro with-lexer-tables ((tables) &body body)
  `(let ((*delimiters* (tables-delimiters ,tables))
	 (*mult-delimiters* (tables-mult-delimiters ,tables))
	 (*key-words* (tables-key-words ,tables))
	 (*constants* (make-instance 'const-table))
	 (*identifiers* (make-instance 'ids-table))
	 (*errors*))
     ,@body))

(defmacro with-lexing (&body body)
  `(with-lexer-tables (,(load-tables *path-to-info*))
     ,@body))


(defun multi-cons (lst1 lst2)
  (if lst1
      (if (listp lst1)
	  (cons (first lst1) (multi-cons (rest lst1) lst2))
	  (cons lst1 lst2))
      lst2))

(defun is-ws? (char)
  (char<= char #\space))

(defun is-number? (char)
  (and (char>= char #\0) (char<= char #\9)))

(defun is-delimiter? (char)
  (gethash char *delimiters*))

(defun is-mult-delimiter? (str)
  (gethash str *mult-delimiters*))

(defun is-leter? (char)
  (and (char-not-lessp char #\a) (char-not-greaterp char #\z)))

(defun get-string (lst)
  (coerce lst 'string))

(defparameter *stream* nil)

(defun lexical-analyze (pathname)
  (let ((tables (load-tables *path-to-info*)))
    (with-open-file (*stream* pathname)
      (with-lexer-tables (tables)
	(char-reader)))))

(defun read-delimiter (char)
  (check-type char character)
  (if (or (char= char #\.) (char= char #\:))
      (let* ((next-ch (read-char *stream* nil))
	     (res (when next-ch
		    (is-mult-delimiter? (coerce `(,char ,next-ch) 'string)))))
	(if res res
	    (if (char= char #\:)
		(is-delimiter? char)
		(error "Unnown shit!~%"))))
      (gethash char *delimiters*)))

(defun read-number (lst)
  (let ((ch (read-char *stream* nil)))
    (cond
      ((is-number? ch)
       (read-number (cons ch lst)))
      ((or (null ch) (is-ws? ch))
       (add-element *constants*
		    (coerce (reverse lst) 'string)))
      ((is-delimiter? ch)
       (list
	(read-delimiter ch)
	(add-element *constants* (coerce (reverse lst) 'string)))))))

(defun read-identifier (lst)
  (let ((ch (read-char *stream* nil)))
    (cond
      ((or (is-number? ch) (is-leter? ch))
       (read-identifier (cons ch lst)))
      ((or (null ch) (is-ws? ch))
       (add-element *identifiers* (coerce (reverse lst) 'string)))
      ((is-delimiter? ch)
       (list
	(read-delimiter ch)
	(add-element *identifiers* (coerce ())))))))

(defun char-reader ()
  (labels ((%char-reader (&optional lst line col)
	     (let ((ch (read-char *stream* nil)))
	       ;;(format T "~A~%" ch)
	       (if ch
		   (cond
		     ((char= ch #\newline)
		      (%char-reader lst (1+ line) 0))
		     ((char<= ch #\space)
		      ;;(format T "~%")
		      (%char-reader lst line (1+ col)))
		     ((and (char>= ch #\0) (char<= ch #\9))
		      (%char-reader (multi-cons (read-number `(,ch)) lst)
				    line (1+ col)))
		     ((is-delimiter? ch)
		      (%char-reader (multi-cons (read-delimiter ch) lst)
				    line (1+ col)))
		     ((is-leter? ch)
		      (%char-reader lst line (1+ col)))
		     (T (error "Invalid character in:~%line: ~A ; column: ~A~%"
			       line col)))
		   (progn
		     (format T "~A~%~A~%" (table *constants*) (table *identifiers*))
		     (reverse lst))))))
    (%char-reader nil 1 1)))
