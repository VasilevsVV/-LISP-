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

;;CLASSES++++++++++++++++++++++++++++++++++++++

(defclass table-mixin ()
  ((table :initarg :table
	  :initform (make-hash-table :test 'equal)
	  :reader table)))

(defclass id-mixin ()
  ((current-id :initarg :current-id
	       :initform 0
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
	(prog1 (setf (gethash value table) (get-id obj))
	       (set-id (1+ (get-id obj)) obj)))))

(defclass char-position ()
  ((line :initarg :line
	 :initform 1
	 :reader line
	 :writer set-line)
   (column :initarg :column
	   :initform 1
	   :reader column
	   :writer set-colunm)))

(defgeneric inc (obj))
(defmethod inc ((obj char-position))
  (set-colunm (1+ (column obj)) obj))

(defgeneric incn (obj num))
(defmethod incn ((obj char-position) num)
  (set-colunm (+ (column obj) num) obj))

(defgeneric new-line (obj))
(defmethod new-line ((obj char-position))
  (set-colunm 1 obj)
  (set-line (1+ (line obj)) obj))

(defgeneric set-pos (obj line column))
(defmethod set-pos ((obj char-position) line column)
  (set-line line obj)
  (set-colunm column obj))

(defclass line ()
  ((line :initarg :line
	 :initform ""
	 :reader line
	 :writer set-line)))

(defgeneric push-back (obj &rest chars))
(defmethod push-back ((obj line) &rest chars)
  (set-line (concatenate 'string (line obj) chars) obj))
;;CLASSES-------------------------------------------

;;WITH-MACROSES+++++++++++++++++++++++++++++++++++++

(defparameter *delimiters* nil)
(defparameter *mult-delimiters* nil)
(defparameter *key-words* nil)
(defparameter *constants* nil)
(defparameter *identifiers* nil)
(defparameter *errors* nil)
(defparameter *position* nil)
(defparameter *line* nil)
(defparameter *stream* nil)

(defmacro with-lexer-tables ((tables) &body body)
  `(let ((*delimiters* (tables-delimiters ,tables))
	 (*mult-delimiters* (tables-mult-delimiters ,tables))
	 (*key-words* (tables-key-words ,tables))
	 (*constants* (make-instance 'const-table :current-id 501))
	 (*identifiers* (make-instance 'ids-table :current-id 1001))
	 (*errors* (make-hash-table))
	 (*position* (make-instance 'char-position))
	 (*line* (make-instance 'line)))
     ,@body))

(defmacro with-lexing (&body body)
  `(with-lexer-tables (,(load-tables *path-to-info*))
     ,@body))

;;WITH-MACROSES-------------------------------------

;;HELPERS+++++++++++++++++++++++++++++++++++++++++++

;; (defun multi-cons (lst1 lst2)
;;   (if lst1
;;       (if (listp lst1)
;; 	  (cons (first lst1) (multi-cons (rest lst1) lst2))
;; 	  (cons lst1 lst2))
;;       lst2))

(defun multi-cons (lst1 lst2)
  (labels ((%get-list (lst)
	     (when lst
	       (let ((f (first lst)))
		 (if (listp f)
		     (append (%get-list f) (%get-list (rest lst)))
		     (cons f (%get-list (rest lst)))))))
	   (%multi-cons (lst)
	     (if lst
		 (cons (first lst) (%multi-cons (rest lst)))
		 lst2)))
    (if (listp lst1)
	(%multi-cons (%get-list lst1))
	(cons lst1 lst2))))

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

(defun is-newline? (char)
  (char= char #\newline))

(defun get-string (lst)
  (string-upcase (coerce (reverse lst) 'string)))

(defun add-identifier (str)
  (let ((res (gethash str *key-words*)))
    (if res
	res
	(add-element *identifiers* str))))

;; (defun push-back (str &rest char)
;;   (concatenate 'string str char))

(defun make-pointer (pos &optional lst)
  (if (<= pos 1)
      (coerce (reverse (cons #\^ lst)) 'string)
      (make-pointer (1- pos) (cons #\space lst))))

(defun throw-error (message line stream pos)
  (error "~A~%at line: ~A~%\"~A~A\"~% ~A"
	 message (line pos) line (read-line stream)
	 (make-pointer (column pos))))

(defun inc-line (&rest chars)
  (incn *position* (length chars))
  (eval `(push-back *line* ,@chars)))

(defun reset-line ()
  (new-line *position*)
  (set-line "" *line*))

(defun read-something (char)
  (cond
    ((is-newline? char)
     (reset-line))
    ((is-ws? char)
     (inc-line char))
    ((is-delimiter? char)
     (read-delimiter char))
    ((is-number? char)
     (read-number char))
    ((is-leter? char)
     (read-identifier char))
    (T
     (throw-error "Invalid character" (push-back *line* char)
		  *stream* *position*))))

;;HELPERS------------------------------------------

(defun read-delimiter (char)
  (check-type char character)
  (if (or (char= char #\.) (char= char #\:))
      (let* ((next-ch (read-char *stream* nil))
	     (res (when next-ch
		    (is-mult-delimiter? (get-string `(,next-ch ,char))))))
	(if res
	    (progn (inc-line char next-ch) res)
	    (if (char= char #\:)
		(prog2
		  (inc-line char)
		  (list (is-delimiter? char) (read-something next-ch)))
		(error "Unnown shit!~%"))))
      (progn
	(inc-line char)
	(gethash char *delimiters*))))

(defun read-number (lst)
  (let ((ch (read-char *stream* nil)))
    (cond
      ((is-number? ch)
       (inc-line ch)
       (read-number (cons ch lst)))
      ((is-newline? ch)
       (reset-line)
       (add-element *constants* (get-string lst)))
      ((or (null ch) (is-ws? ch))
       (inc-line ch)
       (add-element *constants* (get-string lst)))
      ((is-delimiter? ch)
       (list
	(read-delimiter ch)
	(add-element *constants* (get-string lst))))
      (T
       (throw-error "Invalid numeric literal" (push-back *line* ch)
		    *stream* *position*)))))

(defun read-identifier (lst)
  (let ((ch (read-char *stream* nil)))
    (cond
      ((or (is-number? ch) (is-leter? ch))
       (inc-line ch)
       (read-identifier (cons ch lst)))
      ((is-newline? ch)
       (reset-line)
       (add-identifier (get-string lst)))
      ((or (null ch) (is-ws? ch))
       (inc-line ch)
       (add-identifier (get-string lst)))
      ((is-delimiter? ch)
       (list
	(read-delimiter ch)
	(add-identifier (get-string lst))))
      (T
       (throw-error "Invalid identifier" (push-back *line* ch)
		    *stream* *position*)))))

(defun char-reader ()
  (labels ((%char-reader (lst)
	     (let ((ch (read-char *stream* nil)))
	       (if ch
		   (cond
		     ((char= ch #\newline)
		      ;;(new-line *position*)
		      ;;(set-line "" *line*)
		      (reset-line)
		      (%char-reader lst))
		     ((char<= ch #\space)
		      (inc-line ch)
		      (%char-reader lst))
		     ((is-number? ch)
		      (inc-line ch)
		      (%char-reader (multi-cons (read-number `(,ch)) lst)))
		     ((is-delimiter? ch)
		      (%char-reader (multi-cons (read-delimiter ch) lst)))
		     ((is-leter? ch)
		      (inc-line ch)
		      (%char-reader (multi-cons (read-identifier `(,ch)) lst)))
		     (T
		      (throw-error "Invalid character" (push-back *line* ch) *stream* *position*)))
		   (progn
		     (format T "~A~%~A~%"
			     (table *constants*)
			     (table *identifiers*))
		     (reverse lst))))))
    (%char-reader nil)))

(defun lexical-analyze (pathname)
  (let ((tables (load-tables *path-to-info*)))
    (with-open-file (*stream* pathname)
      (with-lexer-tables (tables)
	(char-reader)))))
