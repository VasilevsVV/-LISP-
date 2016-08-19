(load "Tsura_dfa.lsp")

(print (setf ex (first (second (parse "10 + 20;")))))

(defun get-expr (lst)

  ;;returns an expression from parsed
  (cond
    ((null (first lst))
     NIL)

    ((listp (first lst))
     (append () (list (get-expr (first lst))) (get-expr (rest lst))))
    
    ((or (equal (first lst) 'num) (equal (first lst) 'id))
     (second lst))
    
    ((equal (first lst) 'expr-add)
     (append '(+) (get-expr (rest lst))))

    ((equal (first lst) 'expr-sub)
     (append '(-) (get-expr (rest lst))))

    ((equal (first lst) 'expr-mul)
     (append '(*) (get-expr (rest lst))))

    ((equal (first lst) 'expr-div)
     (append '(div) (get-expr (rest lst))))

    (T NIL)))

(defun eval-parsed (tree &optional hash)
  ;;evaluate parsed
  (let ((ex (get-expr tree)))
    (if (listp ex) 
	 (if hash
	     (create-line (hash-max-eval (create-tree ex) hash))
	     (create-line (max-eval (create-tree ex))))
	 (list ex))))

(defun del-deadcode (gr)
  (labels ((%erase (vertex)
	     (unless (cl-graph:parent-vertexes vertex)
		 (let ((childs (cl-graph:child-vertexes vertex)))
		   (cl-graph:delete-vertex gr vertex)
		   (dolist (ver childs)
		     (%erase ver))))))
    
    (let ((roots (cl-graph:graph-roots gr)))
      (if (> (length roots) 1)
	  (dolist (root roots)
	    (when (node-p (cl-graph:element root))
	      (%erase root)
	      ))))))

(defun repl-var (lst hash)
  ;;repleace variables in parsed with new numbers, if they exist.
  (cond
    ((null (first lst))
     NIL)

    ((listp (first lst))
     (append () (list (repl-var (first lst) hash)) (repl-var (rest lst) hash)))
    
    ((equal (first lst) 'id)
     (aif (gethash (second lst) hash)
	  (list 'num it)
	  lst))
    
    ((equal (first lst) 'num)
     lst)

    (T (append () (list (first lst)) (repl-var (rest lst) hash)))))

(defun collect-by-id (gr lst)
  (when lst
    (let ((vert (find-by-id gr (first lst))))
      (if vert
	  (cons vert (collect-by-id gr (rest lst)))
	  (collect-by-id gr (rest lst))))))

(defun rewriter (gr vertex)
  (let ((var-hash (make-hash-table :test #'equal)) (variables (gen-alive vertex)))
    (if variables
	(progn
	  (dolist (var variables)
	    (aif (gethash var (node-in (cl-graph:element vertex)))
		 (progn
		     (let ((sources (collect-by-id gr it)))
		       (when (and sources (= (length sources) 1))
			 (let ((evaled (eval-parsed	  
					(second (second (first (second
								(node-parsed (cl-graph:element (first sources))))))))))
			   (if (numberp (first evaled))
			       (setf (gethash var var-hash) (first evaled))
			       (return))))))))

	  (repl-var (node-parsed (cl-graph:element vertex)) var-hash))
	nil)))

(defun eliminator (gr rewriter)
  (let ((visit-h (make-hash-table :test #'equal)) (rep-flag T))
    (labels ((eliminate (gr &optional (vertex (find-vertex gr 'entry)))

	       (let ((elem (cl-graph:element vertex)))
		 (if (gethash vertex visit-h)
		     gr
		     (progn
		       (setf (gethash vertex visit-h) T)
		       
		       (if (node-p elem)   
			   (let ((childrens (cl-graph:child-vertexes vertex)) (rewrite (funcall rewriter gr vertex)))
			     (when (and rewrite (not (equal rewrite (node-parsed elem))))
			       (progn
				 (setf (node-parsed elem) rewrite)
				 (setf rep-flag T)
				 ))

			     (when (equal (node-type elem) '$if)
			       (let ((expression (eval-parsed (first (second (node-parsed elem)))))
				     (source (cl-graph:source-edges vertex)))
				 (when (= (length source) 2)
				   (when (numberp (first expression))
				     (progn
				       (if (= (first expression) 0)
					   (cl-graph:delete-edge gr (first source))
					   (cl-graph:delete-edge gr (second source)))
				       (let ((child (first (cl-graph:child-vertexes vertex))) (parents (cl-graph:parent-vertexes vertex)))
					 (dolist (ver parents)
					   (cl-graph:add-edge-between-vertexes gr ver child)))
				       (cl-graph:delete-vertex gr vertex)
				     )))))
			     
			     (dolist (ver childrens)
			       (eliminate gr ver)))
			   
			   (let ((childrens (cl-graph:child-vertexes vertex)))
			     (dolist (ver childrens)
			       (eliminate gr ver))
			     gr))
		       ;;(print elem)
		       )))))

      (loop
	(setf rep-flag NIL)
	(setf visit-h (make-hash-table :test #'equal))
	(eliminate gr)
	(print 'LOOP)
	(del-deadcode gr)
	(unless rep-flag
	  (return gr)))
      
      ;;(eliminate gr)

      )))
  

;; (setf cfg (build-cfg '($block ($stmt "const x := 30;")
;; 		       ($stmt "const y := 20;")
;; 		       ($if "x - y;" ($stmt "const y := 10 - x;") ($stmt "const b := 100 - y;"))
;; 		       ($while "y + b;" ($block ($stmt "const i := i - 1;") ($if "i;" ($stmt "const y := a - b;") ($continue))))
;; 		       ($stmt "const y := a * b;")
;; 		       ($stmt "const z := x;"))))

(setf cfg (build-cfg '($block ($stmt "const x := 20;")
		       ($stmt "const y := 20;")
		       ($stmt "const e := 20;")
		       ($stmt "const b := 100;")
		       ($if "x - y;" ($block ($stmt "const y := 10 - x;") ($stmt "const b := sum - 100;"))
			($if "x - e;" ($stmt "const a := 100 - y;") ($block ($stmt "const b := 50 - x;") ($stmt "const flag := 1;"))))
		       
		       ($stmt "const y := a * b;")
		       ($stmt "const z := x;"))))

(dfs cfg 'forward #'mid-reach #'f-out-reach #'gen-reach #'kill-reach)

(eliminator cfg #'rewriter)

(cl-graph:graph->dot cfg "DFA-elim.dot" :edge-labeler nil :vertex-labeler
		     (make-labeler))
