(in-package :lexic)

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

(defun make-pointer (pos &optional lst)
  (if (<= pos 1)
      (coerce (reverse (cons #\^ lst)) 'string)
      (make-pointer (1- pos) (cons #\space lst))))

(defun throw-error (message line stream pos)
  (error "~A~%at line: ~A~%\"~A~A\"~% ~A"
	 message (line pos) line (read-line stream nil)
	 (make-pointer (column pos))))

(defun get-string (lst)
  (string-upcase (coerce (reverse lst) 'string)))


(defun inc-line (&rest chars)
  (incn *position* (length chars))
  (eval `(push-back *line* ,@chars)))

(defun reset-line ()
  (new-line *position*)
  (set-line "" *line*))

(defun reverse-hash-table (table)
  (let ((res-hash (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
	       (setf (gethash v res-hash) k))
	     table)
    res-hash))

(defun print-hash-line (stream key val)
  (format stream "~4T~A  ->  ~A~%" key val))

(defgeneric dump-lexic-result (res))
(defmethod dump-lexic-result ((res lexical-result))
  (with-open-file (stream "lexic-result.txt" :direction :output
					     :if-exists :supersede
					     :if-does-not-exist :create)
    (format stream "DELIMITERS:~%")
    (maphash (lambda (k v) (print-hash-line stream k v))
	     (lexical-result-delimiters res))
    (format stream "~%MULT-DELIMITERS:~%")
    (maphash (lambda (k v) (print-hash-line stream k v))
	     (lexical-result-mult-delimiters res))
    (format stream "~%KEYWORDS:~%")
    (maphash (lambda (k v) (print-hash-line stream k v))
	     (lexical-result-key-words res))
    (format stream "~%CONSTANTS:~%")
    (maphash (lambda (k v) (print-hash-line stream k v))
	     (lexical-result-constants res))
    (format stream "~%IDENTIFIERS:~%")
    (maphash (lambda (k v) (print-hash-line stream k v))
	     (lexical-result-identifiers res))
    (format stream "~%LIST OF LEXEMS:~%~{~A~^,~^ ~}."
	    (lexical-result-coded-list res))))
