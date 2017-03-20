(in-package :lexic)

;;STRUCTURES+++++++++++++++++++++++++++

(defstruct lexical-result
  (delimiters nil :type hash-table)
  (mult-delimiters nil :type hash-table)
  (key-words nil :type hash-table)
  (constants nil :type hash-table)
  (identifiers nil :type hash-table)
  (coded-list nil :type list))

(defstruct tables
  (delimiters (make-hash-table :test 'equal) :type (or nil hash-table))
  (mult-delimiters (make-hash-table :test 'equal) :type (or nil hash-table))
  (key-words (make-hash-table :test 'equal) :type (or nil hash-table))
  (constants (make-hash-table :test 'equal) :type (or nil hash-table))
  (identifiers (make-hash-table :test 'equal) :type (or nil hash-table)))

;;CLASSES++++++++++++++++++++++++++++++

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

