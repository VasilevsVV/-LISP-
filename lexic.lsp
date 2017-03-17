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

(defmethod print-object ((object hash-table) stream)
  (format stream "#<~A :TEST ~A :COUNT ~A>~%"
	  (type-of object) (hash-table-test object) (hash-table-count object))
  (maphash (lambda (k v)
	     (format stream "~4T(~A -> ~A)~%" k v))
	   object))

(defun load-table-from-file (stream start-id &optional end-id)
  (let ((hash (make-hash-table :test `equal)))
    (labels ((%load-table (id)
	       (let ((line (read-line stream nil)))
		 (when line
		   ;;(format T "~A -> ~S~%" id line)
		   (if (or (null end-id) (<= id end-id))
		       (setf (gethash (string-upcase line) hash) id)
		       (error "Id ~A is out of range ~A" id (list start-id end-id)))
		   (%load-table (1+ id))))))
      (%load-table start-id)
      hash)))

(defun load-tabels (path)
  (make-tables
   :delimiters
   (with-open-file (stream (concatenate 'string path "/delimiters.info"))
     (load-table-from-file stream 0 255))
   :mult-delimiters
   (with-open-file (stream (concatenate 'string path "/mult-delimiters.info"))
     (load-table-from-file stream 301 400))
   :key-words
   (with-open-file (stream (concatenate 'string path "/keywords.info"))
     (load-table-from-file stream 401 500))))

(defparameter *delimiters* nil)
(defparameter *mult-delimiters* nil)
(defparameter *key-words* nil)
(defparameter *constants* nil)
(defparameter *identifiers* nil)

(defmacro with-lexer-tables ((tables) &body body)
  `(let ((*delimiters* (tables-delimiters ,tables))
	 (*mult-delimiters* (tables-mult-delimiters ,tables))
	 (*key-words* (tables-key-words ,tables))
	 (*constants* (make-hash-table :test 'equal))
	 (*identifiers* (make-hash-table :test 'equal)))
     ,@body))

(defun lexical-analyze (pathname info-dir)
  (let ((tables (load-tabels info-dir)))
    (with-open-file (stream pathname)
      (with-lexer-tables (tables)
	(char-reader stream)))))

(defun char-reader (stream)
  (labels ((%char-reader ()
	     (let ((ch (read-char stream nil)))
	       (when ch
		 (cond
		   ((char<= ch #\space)
		    ;;(format T "~%")
		    (%char-reader))
		   ((and (char>= ch #\0) (char<= ch #\9))
		    (%char-reader))
		   (T
		    (format T "~S~%" ch)))
		 (%char-reader)))))
    (%char-reader)))
