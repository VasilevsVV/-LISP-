(defstruct nd
	name
	childs
	)

(defun div (var1 var2)
  (truncate (/ var1 var2)))

(defun create-tree (line)
  (make-nd 
   :name (car line)
   :childs (mapcar (lambda (lst) (if (listp lst) 
				     (create-tree lst) 
				     (make-nd :name lst :childs nil)) ) 
		   (cdr line))))



(defun create-line (tree)
  (append (list (nd-name tree)) (mapcar (lambda (tr) 
	  (if (null (nd-childs tr)) (nd-name tr) (create-line tr))) 
	      (nd-childs tree))))

(defun print-tree (tree) 
  (print (create-line tree)))

(defun travers-pre (tree fun)
  (make-nd 
   :name (funcall fun (nd-name tree))
   :childs (mapcar (lambda (tr) 
		     (if (null (nd-childs tr)) 
			 (make-nd 
			  :name (funcall fun (nd-name tr)) 
			  :childs nil) 
			 (travers-pre tr fun)))
		   (nd-childs tree))))

(defun travers-post (tree fun)
  (make-nd 
   :childs (mapcar (lambda (tr) 
		     (if (null (nd-childs tr)) 
			 (make-nd 
			  :name (funcall fun (nd-name tr)) 
			  :childs nil) 
			 (travers-post tr fun))
		     ) (nd-childs tree))
   :name (funcall fun (nd-name tree))))

(defun mapTree (tree trav fun)
  (funcall trav tree fun))

(defun eval-tree (tree)
  (let ((line (create-line tree)))
    (if (numberp (nd-name tree))
	(nd-name tree)
	(eval (create-line tree)))))

(defparameter *tree* (create-tree '(+ 1 2 (* 3 5 c a) (- 10 2 a))))
(defparameter *tree1* (create-tree '(+ 1 2 (* 3 5) (- 10 2))))

(defparameter *tree2* (create-tree '(+ 1 a (- ?a c) (+ 5 (- a 4)))))
(defparameter *tree3* (create-tree '(+ 1 a (- 10 c) ?b))) 

(defun eval-nd (nd)
  (let* ((operation (nd-name nd))
	 (childrens (nd-childs nd))
	 (numbers (mapcar #'nd-name (remove-if (lambda (x) (not (numberp (nd-name x)))) childrens)))
	 (not-numbers (remove-if (lambda (x) (numberp (nd-name x))) childrens)))
    (if not-numbers
	(make-nd :name operation
		   :childs
		   (if numbers
		       (cons (make-nd :name (if (= (length numbers) 1)
						  (first numbers)
						  (apply operation
							 numbers))
					:childs nil)
			     not-numbers)
		       not-numbers))
	(make-nd :name (apply operation numbers)
		   :childs nil))))

(defun max-eval (tree)
  (if (nd-childs tree)
      (eval-nd (make-nd
	   :name (nd-name tree)
	   :childs (reduce (lambda (acc x)
			     (if (equal (nd-name tree) (nd-name x))
				 (if acc
				     (append acc (nd-childs x))
				     (nd-childs x))
				 (if acc
				     (append acc (list x))
				     (list x))))
			   (nd-childs tree)
			   :initial-value nil
			   :key #'max-eval)))
      tree))



  
(defun hash-max-eval (tree values)
	(max-eval
		(mapTree tree #'travers-pre (lambda (x)  (if (gethash x values) (gethash x values) x)))))

(defun tree-eq (tree1 tree2)
  (let ((tr1 (create-line tree1)) (tr2 (create-line tree2)))
    (equal tr1 tr2)))

(defun tree-eq-temp (tree1 tree2)
  (if (and tree1 tree2)
      (if (and (listp tree1) (listp tree2))
	  (and (tree-eq-temp (first tree1) (first tree2)) (tree-eq-temp (rest tree1) (rest tree2)))
	  (if (or (and (equal (nd-name tree1) '?) tree2) (and (equal (nd-name tree2) '?) tree1))
	      T
	      (if (equal (nd-name tree1) (nd-name tree2))
		  (tree-eq-temp (nd-childs tree1) (nd-childs tree2))
		  nil))
	  )
      T))

(defun tree-eq-hash (tree1 tree2 &optional (acc-h (make-hash-table)))
  (if (and tree1 tree2)
      (if (and (listp tree1) (listp tree2))
	  (tree-eq-hash (rest tree1) (rest tree2) (tree-eq-hash (first tree1) (first tree2) acc-h))
	  (let ((name1 (nd-name tree1)) (name2 (nd-name tree2)))
	    (cond
	      ((null acc-h) NIL)

	      ((equal name1 name2)
	       (tree-eq-hash (nd-childs tree1) (nd-childs tree2) acc-h))

	      ((and (equal (char (write-to-string name1) 0) #\?) (equal (char (write-to-string name2) 0) #\?))
	       acc-h)
	      
	      ((and (equal (char (write-to-string name1) 0) #\?)
		    (alpha-char-p (char (write-to-string name1) 1)) tree2)
	       (progn 
		 (setf (gethash name1 acc-h) tree2)
		 acc-h))
	      
	      ((and (equal (char (write-to-string name2) 0) #\?)
		    (alpha-char-p (char (write-to-string name2) 1)) tree1)
	       (progn 
		 (setf (gethash name1 acc-h) tree2)
		 acc-h))
	      
	      (T NIL)
	      )))
      (if (null tree2)
	  acc-h
	  NIL)))

(defparameter hash-vals (make-hash-table))
(setf (gethash 'c hash-vals) 5)
(setf (gethash 'a hash-vals) 2)

(defun run-test ()
  (print 'create-tree-------------------------)
  (print (create-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
  (print '-------------------------)
  (print-tree (create-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
  (print 'travers-pre-------------------------)
  (travers-pre *tree* #'print)
  (print 'travers-post-------------------------)
  (travers-post *tree* #'print)
  (print 'maptree-------------------------)
  (maptree *tree* #'travers-pre #'print)
  (print 'eval-tree-------------------------)
  (print (eval-tree *tree1*))
  (print 'max-eval-------------------------)
  (print (create-line (max-eval *tree1*)))
  (print 'hash-max-eval-------------------------)
  (print (create-line (hash-max-eval *tree* hash-vals)))
  (print 'tree-equal-------------------------)
  (print (tree-equal *tree2* *tree3*))
  (print 'tree-eq-temp-------------------------)
  (print (tree-eq-temp *tree2* *tree3*))
  (print 'tree-eq-hash-------------------------)
  (print (tree-eq-hash *tree2* *tree3*))
)


