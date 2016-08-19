(load "Tsura_graph.lsp")
(load "Tsura_parser_v2.lsp")
(load "calcu.lisp")


"(let ((out-h (node-out elem)))
			       (dolist (var (kill (node-parsed vertex)))
				 (setf (gethash var out-h) NIL))
			       (dolist (var (gen (node-parsed vertex)))
				 (setf (gethash (first var) out-h) (rest var))))"

(defun print-hash-entry (key value)
  (format NIL "~S -> ~S~%" key value))

(defun print-hash (hash &optional)
  (let ((ret))
    (if hash
	(maphash (lambda (key val) (setf ret (append ret (list (format NIL "~A -> ~A" key val))))) hash))
    ret))

(defmacro aif (expr t-branch &optional nill-branch)
  (let* ((tmp-sym (gensym)))
    `(let ((,tmp-sym ,expr))
      (if ,tmp-sym 
        (let ((it ,tmp-sym))
          ,t-branch)
        ,nill-branch))))

(defun parse (str)
  (parser (lexer str) str))

(defmacro make-labeler ()
  ;;labeler for making graph.dot
  `(lambda (ver stream) (let ((elem (cl-graph:element ver)))
			  (if (node-p elem)
			      (format stream "~A~%~A: ~A~%In: ~A~%Out: ~A~%~A" (node-id elem) (node-type elem) (node-line elem)
				      (print-hash (node-in elem)) (print-hash (node-out elem)) (node-parsed elem))
			      (format stream "~A" elem)))))

(defmacro make-labeler-alive ()
  ;;labeler for making graph.dot
  `(lambda (ver stream) (let ((elem (cl-graph:element ver)))
			  (if (node-p elem)
			      (format stream "~A: ~A~%In: ~A~%Out: ~A~%~A" (node-type elem) (node-line elem)
				      (node-in elem) (node-out elem) (node-parsed elem))
			      (format stream "~A" elem)))))

(defun copy-hash (hash)
  (let ((ret (make-hash-table :test #'equal)))
    (when hash
      (maphash (lambda (key val) (setf (gethash key ret) val))
	       hash)
      ret)))

;;====================================================================================
(defun gen-reach (vertex)
  (let* ((elem (cl-graph:element vertex)) (temp (first (second (node-parsed elem)))))
    (when (equal (first temp) 'def)
      (list (cons (second (first (second temp))) (node-id elem))))))

(defun kill-reach (vertex)
  (let* ((elem (cl-graph:element vertex)) (temp (first (second (node-parsed elem)))))
    (when (equal (first temp) 'def)
      (list (second (first (second temp)))))))

(defun mid-reach (parents)
  (let ((mid-hash (make-hash-table :test #'equal)))
    ;;(print parents)
    (dolist (ver parents)
      (when (and (node-p (cl-graph:element ver)) (node-out (cl-graph:element ver)))
	(maphash (lambda (key val)
		   (let ((get (gethash key mid-hash)))
		     (setf (gethash key mid-hash) (union get val :test #'equal))))
		 (node-out (cl-graph:element ver)))))
    mid-hash))

(defun f-out-reach (vertex gen kill)
  (let* ((elem (cl-graph:element vertex)) (out-h (make-hash-table :test #'equal)))
    (setf out-h (copy-hash (node-in elem)))
    ;;(print (print-hash out-h))
    ;;(print kill)
    ;;(print gen)
    (if gen
	(progn
	  
	  (when (node-in elem)
	    (dolist (tmp kill)
	      ;;(print (print-hash out-h))
	      ;;(print '----)
	      ;;(print (gethash tmp out-h))
	      (when (gethash tmp out-h)
		(progn
		  
		  ;;(format t "~%~A; ~A; ~A" 'KILL tmp (gethash tmp out-h))
		  ;;(print (print-hash out-h))
		  (remhash tmp out-h)))))
	  (dolist (tmp gen)
	    (setf (gethash (first tmp) out-h) (list (rest tmp))))
	  out-h)
	(node-in elem))))
;;====================================================================================
"(defun get-gen (lst)
  ;;Return a list of generated symbols.
  (let ((temp (first (second lst))))
    (when (equal (first temp) 'def)
      (list (second temp)))))"

(defun eq-sets (lst1 lst2)
  (not (xor lst1 lst2)))

(defun gen-alive (vertex)
  (labels ((%gen (lst)
	     (cond
	       ((null (first lst)) nil)
	       ;;-------------------------------
	       ((listp (first lst))
		(append () (%gen (first lst)) (%gen (rest lst))))
	       ;;-------------------------------
	       ((equal (first lst) 'id)
		(list (second lst)))
	       ;;-------------------------------
	       ((equal (first lst) 'def)
		(%gen (rest (first (rest lst)))))
	       ;;-------------------------------
	       ((equal (first lst) 'stmt)
		(%gen (first (second lst))))
	       ;;-------------------------------
	       (T
		(append () (%gen (rest lst)))))
	     ))
    (%gen (node-parsed (cl-graph:element vertex)))
    ))
	   
(defun kill-alive (vertex)
  (let ((temp (first (second (node-parsed (cl-graph:element vertex))))))
    (when (equal (first temp) 'def)
      (list (second (first (second temp)))))))

(defun mid-alive (childs)
  (when childs
    (reduce (lambda (acc x)
	      (union acc (when (node-p (cl-graph:element x)) (node-in (cl-graph:element x))) :test #'equal))
	    childs
	    :initial-value nil)))

(defun f-in-alive (vertex gen kill)
  (union gen (set-difference (node-out (cl-graph:element vertex)) kill :test #'equal) :test #'equal))


(defun dfs (gr direction mid f-out gen kill)
  (let ((visit-h (make-hash-table :test #'equal)) (rep-flag nil))
    (labels ((dfs_forward (gr &optional (vertex (find-vertex gr 'entry)))
	       
	       (let ((elem (cl-graph:element vertex)))
		 (if (gethash vertex visit-h)
		     gr
		     (progn
		       (setf (gethash vertex visit-h) T)
		       
		       (if (node-p elem)
			   (let ((parents (cl-graph:parent-vertexes vertex)))
			     			     
			     (let ((md (funcall mid parents)))
			       (unless (equalp md (node-in elem))	   
				 (setf rep-flag T))			       
			       (setf (node-in elem) md)
			  
			      )

			     (let ((out-temp (funcall f-out vertex
						      (funcall gen vertex)
						      (funcall kill vertex))))
			       (unless (equalp out-temp (node-out elem))
				 (setf rep-flag T))
			       (setf (node-out elem) out-temp))
			     
			     (let ((childrens (cl-graph:child-vertexes vertex)))
			       (dolist (ver childrens)
				 (dfs_forward gr ver))))
			   (let ((childrens (cl-graph:child-vertexes vertex)))
			     (dolist (ver childrens)
			       (dfs_forward gr ver))
			     gr))))))
	     
	     (dfs_backward (gr &optional (vertex (find-vertex gr 'exit)))
	       (let ((elem (cl-graph:element vertex)))
		 (if (gethash vertex visit-h)
		     gr
		     (progn
		       (setf (gethash vertex visit-h) T)
		       
		       (if (node-p elem)
			   (let ((childs (cl-graph:child-vertexes vertex)))
			     ;;------------------------------
			     (let ((md (funcall mid childs)))			       
			       (unless (equalp md (node-out elem))	   
				 (setf rep-flag T))			       
			       (setf (node-out elem) md))
			     ;;------------------------------
			     (let ((in-temp (funcall f-out vertex
						      (funcall gen vertex)
						      (funcall kill vertex))))
			       (unless (equalp in-temp (node-in elem))
				 (setf rep-flag T))
			       (setf (node-in elem) in-temp))
			     ;;------------------------------
			     (let ((parents (cl-graph:parent-vertexes vertex)))
			       (dolist (ver parents)
				 (dfs_backward gr ver))))
			   ;;-------------------------------------------------
			   (let ((parents (cl-graph:parent-vertexes vertex)))
			     (dolist (ver parents)
			       (dfs_backward gr ver))
			     gr)))))))
      
      (if (equal direction 'forward)
	  (loop
	    
	    (setf rep-flag NIL)
	    (setf visit-h (make-hash-table :test #'equal))
	    (dfs_forward gr)
	    ;;(print rep-flag)
	    (unless rep-flag
	      (return gr)))
	  
	  (loop
	    (setf rep-flag NIL)
	    (setf visit-h (make-hash-table :test #'equal))
	    (setf gr (dfs_backward gr))
	    ;;(print rep-flag)
	    (unless rep-flag
	      (return gr))))
      )))

(defun test-dfa ()

  ;; (setf cfg (build-cfg '($block ($stmt "const z := a + b;")
  ;; 			 ($while "a + b;" ($block ($if "a + b;" ($continue) ($stmt "const a := x - z;"))
  ;; 					   ($stmt "const z := x;") ($stmt "const z := y;")))
  ;; 			 ($stmt "const y := a * b;")
  ;; 			 ($stmt "const z := x;"))))

  ;; (setf cfg (build-cfg '($block ($stmt "const z := ;; a + b;")
  ;; 			 ($if "x - y;" ($stmt "const y := 10 - a;") ($stmt "const b := 100 - 20;"))
  ;; 			 ($stmt "const y := a * b;")
  ;; 			 ($stmt "const z := x;"))))

  (setf cfg (build-cfg '($block ($stmt "const a := 1;")
			 ($while "a;" ($block ($stmt "const b := 2;")
			  ($while "b;" ($block ($if "b;" ($break))
			   ($stmt "const z := 100;")))
			  ($stmt "const a := a;")
			  ($stmt "const a := 2;"))))))
  
  (dfs cfg 'forward #'mid-reach #'f-out-reach #'gen-reach #'kill-reach)
  
  (cl-graph:graph->dot cfg "DFA-reach.dot" :edge-labeler nil :vertex-labeler
		       (make-labeler))
  
  ;; (setf cfg (build-cfg '($block ($stmt "const z := a + b;")
  ;; 			 ($while "a + b;" ($block ($if "a + b;" ($break) ($stmt "const a := x - z;"))
  ;; 					   ($block ($if "a;" ($break) ($stmt "const a := 100;"))) ($stmt "const z := y;")))
  ;; 			 ($stmt "const y := a * b;")
  ;; 			 ($stmt "const z := x;"))))

  
  (setf cfg (build-cfg '($block ($stmt "const z := a + b;")
			 ($if "a;" ($stmt "const z := z + 10;") ($stmt "const y := a - 20;"))
			 ($stmt "const y := a * b;")
			 ($stmt "const z := x;"))))
  
  (dfs cfg 'backward #'mid-alive #'f-in-alive #'gen-alive #'kill-alive)
  
  (cl-graph:graph->dot cfg "DFA-aliveness.dot" :edge-labeler nil :vertex-labeler
		       (make-labeler-alive)))
