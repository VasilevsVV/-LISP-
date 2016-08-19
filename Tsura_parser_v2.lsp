(defmacro mv-let* (binds &body body)
  (labels ((%mv-let (local-binds)
    (if local-binds
      `(multiple-value-bind ,(first (first local-binds)) ,(second (first local-binds)) 
        ,(%mv-let (rest local-binds)))
      `(progn ,@body))))
    (%mv-let binds)))

(defmacro aif (expr t-branch &optional nill-branch)
  (let* ((tmp-sym (gensym)))
    `(let ((,tmp-sym ,expr))
      (if ,tmp-sym 
        (let ((it ,tmp-sym))
          ,t-branch)
        ,nill-branch))))
        
        
(defun pointer (pos)
  "Creates a pointer(arrow) to error"
  (if (> pos 0)
    (concatenate 'string " " (pointer (- pos 1)))
    "^"))

(defun cut-line (str num)
  "Cuts out a needed line from text"
  (let ((pos (position #\; str)))
    (if pos
      (if (= num 0)
        (subseq str 0 (1+ pos))
        (cut-line (subseq str (1+ pos)) (- num 1)))
      str)))
    

(defun lexer (str)
    (analyze (coerce str 'list) '(0 0) str))

(defun analyze (lst &optional (pos (list 0 0)) line)
  "analyzes each symbol"
  (let ((sym (first lst)))
    (cond
      ((null sym)  nil)
      ((char= sym #\( ) (append () (list (cons 'o-parenth pos)) (analyze (rest lst) (list (1+ (first pos)) (second pos)) line)))
      ((char= sym #\) ) (append () (list (cons 'c-parenth pos)) (analyze (rest lst) (list (1+ (first pos)) (second pos)) line)))
      ((< (char-code sym) 33)   (append () nil (analyze (rest lst) (list (1+ (first pos)) (second pos)) line)))
      ((and (> (char-code sym) 47)  (< (char-code sym) 58))
         (let ((nxt (next lst (first pos)))) (append () (list (cons (numb lst pos line) pos)) (analyze (first nxt) (list (second nxt) (second pos)) line))))
      ((char= sym #\+)   (append () (list (cons 'add pos)) (analyze (rest lst) (list (1+ (first pos)) (second pos)) line)))
      ((char= sym #\-)   (append () (list (cons 'sub pos)) (analyze (rest lst) (list (1+ (first pos)) (second pos)) line)))
      ((char= sym #\*)   (append () (list (cons 'mul pos)) (analyze (rest lst) (list (1+ (first pos)) (second pos)) line)))
      ((char= sym #\/)   (append () (list (cons 'div pos)) (analyze (rest lst) (list (1+ (first pos)) (second pos)) line)))
      ((char= sym #\;)   (append () (list (cons 'semicolon pos)) (analyze (rest lst) (list (if (rest lst) (if (char= (second lst) #\newline) -1 0) 0) (1+ (second pos))) line)))
      ((let ((code (char-code sym)))
        (or (and (>= code 65) (<= code 90)) (and (>= code 97) (<= code 122))))
          (let ((nxt (next lst (first pos)))) (append () (list (cons (ident lst pos line) pos)) (analyze (first nxt) (list (second nxt) (second pos)) line))))
      ((char= sym #\:)
        (if (char= (second lst) #\=)
          (append () (list (cons 'assign pos)) (analyze (rest (rest lst)) (list (+ 2 (first pos)) (second pos)) line))))
      (t (error "Unalloved symbol \"~A\" !!~%~A~%~A" sym (cut-line line (second pos)) (pointer (first pos)))))))

      
      
      
      ; (((ID "a") 0 0) ((ASSIGN 2 0) ((NUM 10) 5 0) ((SEMICOLON 7 0))))
      
      
      
     
(defun numb (lst position line &optional acc)
  "Reads the word and cheking it if it's number, Return (num #)"
  (let ((sym (first lst)))
    (cond
      ((or (null sym) (< (char-code sym) 33) (char= sym #\() (char= sym #\)) 
      (char= sym #\+) (char= sym #\-) (char= sym #\*) (char= sym #\/) (char= sym #\;))
         (list 'num (parse-integer (coerce (reverse acc) 'string))))
      ((and (> (char-code sym) 47)  (< (char-code sym) 58))
         (numb (rest lst) position line (cons (first lst) acc)))
      (T (error "Unalloved symbol \"~A\" in number~%~A~%~A" 
        sym (cut-line line (second position)) (pointer (first position)))))))


(defun next (lst &optional pos)
  "NEXT - returns list without current lexem"
  (cond
    ((null lst)   nil)
    ((let ((sym (char-code (first lst))))
      (or (and (>= sym 65) (<= sym 90)) (and (>= sym 97) 
      (<= sym 122)) (and (> sym 47)  (< sym 58))))
        (next (rest lst) (1+ pos)))
    (t  (list lst pos))))

      
(defun ident (lst position line &optional acc)
  "Reads the word and cheking rather it's ident or cons. return (ident #)/const "
  (let ((sym (first lst)))
    (cond
      ((or (null sym) (< (char-code sym) 33) (char= sym #\() (char= sym #\)) 
      (char= sym #\+) (char= sym #\-) (char= sym #\*) (char= sym #\/) (char= sym #\;))
        (let ((str (coerce (reverse acc) 'string)))
          (if (string= str "const")
            'const
            (list 'id str))))
      ((let ((sym-code (char-code sym)))
        (or (and (>= sym-code 65) (<= sym-code 90)) (and (>= sym-code 97) 
        (<= sym-code 122)) (and (> sym-code 47)  (< sym-code 58))))
          (ident (rest lst) position line (cons sym acc)))
      (T (error "Unalloved symbol \"~A\" in identifier~%~A~%~A" 
            sym (cut-line line (second position)) (pointer (first position)))))))

(defun test-lexer ()
  ;(pprint (set 'lex-res (lexer "10 + 20 * 30 / x;")))
  (pprint (setq lex-res (lexer "const x := 10;
  const y := 20;
  const z := 10*(20 + y * 5) * x;
  100 + 15 * 20 / (z - (x + y));")))
)

;----------------------------------------------------

(defun parser (lst text)
  (list 'stmt (stmt lst text)))
  
(defun definition (lst text)
  (let (rest-return)
    (values
      (if (listp (first (first lst)))
        (if (and (equal (first (first (first lst))) 'id) (stringp (second (first (first lst)))))
          (if (equal (first (second lst)) 'assign)
            (mv-let* (((ex rest-lst) (expr (rest (rest lst)) text)))
              (setq rest-return rest-lst)
              (list 'def (list (first (first lst)) ex)))
            (error "The \":=\" expected~%~A~%~A" (cut-line text (third (second lst))) (pointer (second (second lst)))))
          (error "Identifier expected~%~A~%~A" (cut-line text (third (first lst))) (pointer (second (first lst)))))
        (error "Identifier expected~%~A~%~A" (cut-line text (third (first lst))) (pointer (second (first lst)))))
      rest-return)))
  
(defun stmt (lst text)
  (when lst
    (if (equal (first (first lst)) 'const)
      (mv-let* (((def rest-lst) (definition (rest lst) text)))
        (if (equal (first (first rest-lst)) 'semicolon)
          (append () (list def) (stmt (rest rest-lst) text))
          ;(reverse (cons (stmt (rest rest-lst) text) (cons def nil)))
          (if rest-lst
            (error "Semicolon expected~%~A~%~A" (cut-line text (third (first rest-lst))) (pointer (second (first rest-lst))))
            (error "Semicolon expected after:~%~A~%~A" (cut-line text (third (first lst))) (pointer (second (first lst)))))))
      (mv-let* (((ex rest-lst) (expr lst text)))
        (if (equal (first (first rest-lst)) 'semicolon)
          (append () (list ex) (stmt (rest rest-lst) text))
          ;(reverse (cons (stmt (rest rest-lst) text) (cons ex nil)))
          (if rest-lst
            (error "Semicolon expected~%~A~%~A" (cut-line text (third (first rest-lst))) (pointer (second (first rest-lst))))
            (error "Semicolon expected in the end of:~%~A~%~A" (cut-line text (third (first lst))) (pointer (second (first lst))))))))))

(defun expr (lst text)
  (let (ex-list-rest)
    (values
      (mv-let* (((trm lst-rest) (term lst text)) ((exp-list rest) (expr-list lst-rest text)))
        (setq ex-list-rest rest) 
        (if exp-list
          (list (first exp-list) trm (second exp-list))
          trm))
      ex-list-rest)))

(defun term (lst text)
  (let (tm-list-rest)
    (values
      (mv-let* (((fact lst-rest) (factor lst text))  ((trm-list rest) (term-list lst-rest text)))
        (setq tm-list-rest rest)
        (if trm-list
          (list (first trm-list) fact (second trm-list))
          fact))
      tm-list-rest)))

(defun expr-list (lst text)
  (let ((op 
          (cond 
            ((equal (first (first lst)) 'add) 'expr-add) 
            ((equal (first (first lst)) 'sub) 'expr-sub)
            (T nil)))
          ex-list-rest)
    (values
      (if op
        (mv-let* (((trm lst-rest) (term (rest lst) text)) ((exp-list rest) (expr-list lst-rest text)))
          (setq ex-list-rest rest)
          (if exp-list
            (list op (list (first exp-list) trm (second exp-list)))
            (list op trm)))
        (progn (setq ex-list-rest lst) nil))
      ex-list-rest)))

(defun term-list (lst text)
  (let ((op 
          (cond 
            ((equal (first (first lst)) 'mul) 'expr-mul) 
            ((equal (first (first lst)) 'div) 'expr-div)
            (T nil))) 
          tm-list-rest)
    (values
      (if op
        (mv-let* (((fact lst-rest) (factor (rest lst) text)) ((trm-list rest) (term-list lst-rest text)))
          (setq tm-list-rest rest)
          (if trm-list 
            (list op (list (first trm-list) fact (second trm-list)))
            (list op fact)))
        (progn (setq tm-list-rest lst) nil))
      tm-list-rest)))  
 
(defun factor (lst text)
  (let (lst-rest)
    (values
      (cond
        ((listp (first (first lst))) (progn (setq lst-rest (rest lst)) (first (first lst))))
        ((equal (first (first lst)) 'o-parenth)
          (mv-let* (((ex rest) (expr (rest lst) text)))
            (if (equal (first (first rest)) 'c-parenth)
              (progn
                (setq lst-rest (rest rest))
                ex)
              (error "Close parenthesis expected~%~A~%~A" (cut-line text (third (first rest))) (pointer (second (first rest)))))))
        (T (error "Number, identifier or expression expected~%~A~%~A" (cut-line text (third (first lst))) (pointer (second (first lst))))))
      lst-rest)))

      
(defun test-expr (str)
  (expr (lexer str) str)
)
  

;Chenge this parameter to chek some errors!!  
(defparameter code "const x := 10;
  const y := 20;
  const z := 10*(20 + y * 5) * x;
  100 + 15 * 20 / (z - (x + y));")      

(defun run-test ()
  (parser (lexer code) code))
  
(defun test-parser (str)
  (parser (lexer str) str))
      
      
      

