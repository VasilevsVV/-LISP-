(eval-when (:compile-toplevel :load-toplevel)
  (let ((files-paths '("/home/vv/study/-LISP-/lexic-package.lsp"
		       "/home/vv/study/-LISP-/lexic-global-vars.lsp"
		       "/home/vv/study/-LISP-/lexic-classes.lsp"
		       "/home/vv/study/-LISP-/lexic-utils.lsp")))
    (dolist (path files-paths)
      (compile-file path)
      (load path))))

(in-package :lexic)

;;WITH-MACROSES+++++++++++++++++++++++++++++++++++++

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

(defun add-identifier (str)
  (let ((res (gethash str *key-words*)))
    (if res
	res
	(add-element *identifiers* str))))

;;HELPERS------------------------------------------

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
	(let ((res (char-reader)))
	  (make-lexical-result
	   :coded-list res
	   :delimiters (reverse-hash-table *delimiters*)
	   :mult-delimiters (reverse-hash-table *mult-delimiters*)
	   :key-words (reverse-hash-table *key-words*)
	   :constants (reverse-hash-table (table *constants*))
	   :identifiers (reverse-hash-table (table *identifiers*))))))))
