;(rename-package :cl-graph :g)
(ql:quickload "cl-graph")
(load "Tsura_parser_v2.lsp")

(defun parse (str)
  (parser (lexer str) str))

(defstruct node
  id
  type
  line
  parsed
  in
  out
  if-flag)
  
(defmacro find-vertex (gr elem)
  `(cl-graph:find-vertex-if ,gr (lambda (x) (equal (cl-graph:element x) ,elem))))

(defun find-by-id (gr compare)
  (cl-graph:find-vertex-if gr (lambda (x) (when (node-p (cl-graph:element x))
					    (equal (node-id (cl-graph:element x)) compare)))))

(defun find-by-id-type (gr id type)
  (cl-graph:find-vertex-if gr (lambda (x) (when (node-p (cl-graph:element x))
					    (and (equal (node-id (cl-graph:element x)) id) (equal (node-type (cl-graph:element x)) type))))))

(defun break-collector (gr start-point)
  ;;Collect all the break vertexes and targets them to exit
  (loop
    (let ((collector (find-by-id-type gr (node-id (cl-graph:element start-point)) 'break-collector)))
      (if collector
	  (let ((breaks (cl-graph:parent-vertexes collector)) (exit (find-vertex gr 'exit)))
	    (cl-graph:delete-vertex gr collector)
	    (dolist (ver breaks)
	      (let ((source (first (cl-graph:source-edges ver))))
		(when source
		  (progn
		    (warn "Unreachable code after [~A] in [~A] may be deleted." (node-line (cl-graph:element ver))
			  (node-line (cl-graph:element start-point)))
		    (cl-graph:delete-edge gr source))))
	      (cl-graph:add-edge-between-vertexes gr ver exit))
	    gr)
	  (return gr)))))

(defun cont-collector (gr start-point)
  ;;Collect all the continue vertexes and targets them to start of while
  (loop
    (let ((collector (find-by-id-type gr (node-id (cl-graph:element start-point)) 'cont-collector)))
      (if collector
	  (let ((continues (cl-graph:parent-vertexes collector)) (exit (find-vertex gr 'exit)))
	    (cl-graph:delete-vertex gr collector)
	    (dolist (ver continues)
	      (let ((source (first (cl-graph:source-edges ver))))
		(when source
		  (progn
		    (warn "Unreachable code after [~A] in [~A] may be deleted." (node-line (cl-graph:element ver))
			  (node-line (cl-graph:element start-point)))
		    (cl-graph:delete-edge gr source))))
	      (cl-graph:add-edge-between-vertexes gr ver start-point))
	    gr)
	  (return gr)))))

(defun eraser (gr)
  ;;Delets all dead-code after breaks & conts.
  ;;I know, it's bad way to do it, but wright now I don't know how to make it beter.
  (loop
    (let ((roots (cl-graph:graph-roots gr)))
      (dolist (ver roots)
	(when (not (equal (cl-graph:element ver) 'entry))
	  (cl-graph:delete-vertex gr ver)))
      (when (= (length roots) 1)
	  (return gr)))))

(defun graph-conc (graph1 graph2)
  ;;Concatenate two graphs.
  (let* ((gr1 graph1) (gr2 graph2)
	(exit (cl-graph:parent-vertexes (find-vertex gr1 'exit)))
	 (entry (first (cl-graph:child-vertexes (find-vertex gr2 'entry)))))
    
    (cl-graph:delete-vertex gr1 (find-vertex gr1 'exit))
    (cl-graph:delete-vertex gr2 (find-vertex gr2 'entry))

    (cl-graph:iterate-edges gr2
      (lambda (ed) (cl-graph:add-edge-between-vertexes gr1
	  (cl-graph:element (cl-graph:source-vertex ed)) (cl-graph:element (cl-graph:target-vertex ed)))))

    (dolist (ver exit)
      (cl-graph:add-edge-between-vertexes gr1 ver (cl-graph:element entry)))
    gr1))

(defun graph-conc-vert (graph1 link-ver graph2)
  ;;Adjoins a second graph to "link-ver" vertex of first graph.
  (let* 
      ((gr1 graph1) (gr2 graph2)
       ;;(exit (cl-graph:parent-vertexes (cl-graph:find-vertex-if gr1 (lambda (x) (equal (node-id (cl-graph:element x)) 'exit)))))
       (entry (first (cl-graph:child-vertexes (find-vertex gr2 'entry)))))
    
    (cl-graph:delete-vertex gr2 (find-vertex gr2 'entry))

    (cl-graph:iterate-edges gr2
      (lambda (ed) (cl-graph:add-edge-between-vertexes gr1
	  (cl-graph:element (cl-graph:source-vertex ed)) (cl-graph:element (cl-graph:target-vertex ed)))))

    (dolist (ver link-ver)
      (cl-graph:add-edge-between-vertexes gr1 ver (cl-graph:element entry)))
    gr1))


(defun build-cfg (lst &optional start-point)
  ;;Builds CFG from structure tree.
  (let ((type (first lst))
	(gr (cl-graph:make-graph 'cl-graph:dot-graph :default-edge-type :directed)))
    (cond
      ((equal type '$stmt)
       (let ((ver (cl-graph:add-vertex gr (make-node :id (gensym) :type '$stmt :line (second lst) :parsed (parse (second lst)))))
	     (gr (cl-graph:make-graph 'cl-graph:dot-graph :default-edge-type :directed)))
	 (progn
	   (cl-graph:add-edge-between-vertexes gr 'entry ver)
	   (cl-graph:add-edge-between-vertexes gr ver 'exit)
	   gr)))
      ;;----------------------------------------------
      ((equal type '$block)
       (progn
	 (cl-graph:add-edge-between-vertexes gr 'entry 'exit)
	 (dolist (line (rest lst))
	   (setf gr (graph-conc gr (build-cfg line start-point))))
	 gr))
      ;;----------------------------------------------
      ((equal type '$if)
       (let ((if-node (cl-graph:add-vertex gr (make-node
		       :id (gensym)
		       :type type
		       :line (second lst)
		       :parsed (parse (second lst))))))
       (progn
	 (cl-graph:add-edge-between-vertexes gr 'entry if-node)
	 (graph-conc-vert gr (list if-node) (build-cfg (third lst) start-point))
	 (if (fourth lst)
	     (graph-conc-vert gr (list if-node) (build-cfg (fourth lst) start-point))
	     (cl-graph:add-edge-between-vertexes gr if-node (find-vertex gr 'exit)))
	 gr)))
      ;;----------------------------------------------
      ((equal type '$while)
       (let ((while-node (cl-graph:add-vertex gr (make-node
						  :id (gensym)
						  :type type
						  :line (second lst)
						  :parsed (parse (second lst))))))
	 (progn
	   (cl-graph:add-edge-between-vertexes gr 'entry while-node)
	   (graph-conc-vert gr (list while-node) (build-cfg (third lst) while-node))
	   (cl-graph:iterate-parents (find-vertex gr 'exit)
	     (lambda (x) (cl-graph:add-edge-between-vertexes gr x while-node)))
	   (cl-graph:iterate-target-edges (find-vertex gr 'exit)
	     (lambda (x) (cl-graph:delete-edge gr x)))
	   (cl-graph:add-edge-between-vertexes gr while-node (find-vertex gr 'exit))
	   (break-collector gr while-node)
	   (cont-collector gr while-node)
	   (eraser gr)
	   gr)))
      ;;----------------------------------------------
      ((equal type '$break)
       (let ((brk (make-node :id (gensym ) :type type :line 'break)))
	 (progn
	   (cl-graph:add-edge-between-vertexes gr 'entry brk)
	   (if start-point
	       (cl-graph:add-edge-between-vertexes gr brk (make-node :id (node-id (cl-graph:element start-point))
								     :type 'break-collector))
	       (error "ERROR: [break] is not in the [while] body"))
	   (cl-graph:add-edge-between-vertexes gr brk 'exit)
	   gr)))
      ;;----------------------------------------------
      ((equal type '$continue)
       (let ((cont (make-node :id (gensym) :type type :line 'continue)))
	 (progn
	   (cl-graph:add-edge-between-vertexes gr 'entry cont)
	   (if start-point
	       (cl-graph:add-edge-between-vertexes gr cont (make-node :id (node-id (cl-graph:element start-point))
								      :type 'cont-collector))
	       (error "ERROR: [continue] is not in the [while] body"))
	   (cl-graph:add-edge-between-vertexes gr cont 'exit)
	   gr)))
      ;;----------------------------------------------
      (T (error "Unavailable structure of code")))))

;;(print (cl-graph:vertexes (build-cfg '($if "lalka" ($stmt "lalka")))))

(defun test-builder ()
  (pprint (cl-graph:vertexes (setf test-gr (build-cfg
       '($block ($stmt "const z := a + b;") ($stmt "const y := a * b;")
			  ($while "a + b;" ($block ($stmt "const b := a + 1;") ($if "a + x;" ($continue))
					    ($stmt "const x := a + b - z;")))
			  ($if "a - b;" ($stmt "const z := a + 5;") ($stmt "const x := b - 10;")))))))

  (cl-graph:graph->dot test-gr "graph-test.dot" :edge-labeler nil :vertex-labeler
		     (lambda (ver stream) (format stream "~A" (if (node-p (cl-graph:element ver))
								  (node-line (cl-graph:element ver))
								  (cl-graph:element ver))))))
