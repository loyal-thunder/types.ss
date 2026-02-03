(define (kind x)
  (cond ((list? x) 'list)
	((number? x) 'number)
	((string? x) 'string)
	((char? x) 'char)
	((procedure? x) 'procedure)
	(else 'undefined)))

(define (fail msg)
  (raise
   (condition
    (make-error)
    (make-message-condition msg))))


(define-syntax maybe
  (syntax-rules ()
    ((_ '(ident type-name))
     (let ((is (lambda (value)
		 (if (equal? type-name (kind value))
		     value
		     (fail (format (string-append
				    "type mismatch in a variant construction!~%"
				    "Given: ~A~%"
				    "Expected: ~A~%")
				   (kind value)
				   type-name))))))
       (list ident type-name is)))))


(define-syntax type
  (syntax-rules ()
    ((_ (ident . ()))
     (fail "Expected a type name in 'type'!"))
    ((_ (ident . (type-name . ())) . rest)
     (cons (maybe `((quote ident) (quote type-name)))
	   (type . rest)))
    ((_ (ident . type-name))
     (maybe (ident . type-name)))
    ((_ unexpected . rest) (fail "Unexpected value in 'type'!\n"))
    ((_) '())))

(define-syntax instance
  (syntax-rules (of)
    ((_ (ident . (value . ())) of defs)
     (let* ((p (lambda (trip) (equal? (car trip) (quote ident))))
	    (left (filter p defs))
	    (ret? (not (equal? left '()))))
       (if ret?
	   (list (quote ident) (kind value) ((caddar left) value))
	   (fail "Error!\n"))))))

(define (bind-list xs ys)
  (if (null? xs)
      '()
      (cons `(,(car xs) ,(car ys)) (bind-list (cdr xs) (cdr ys)))))

(define (capture-list-f xs ys body)
  (let*  ((make-def (lambda (a b) `(define ,a ,b)))
	 (defs (map (lambda (p) (apply make-def p)) (bind-list xs ys)))
	 (full-expr (append (cons 'begin defs) (list body))))
    (eval full-expr)))

(define-syntax match
  (lambda (x)
    (syntax-case x (of else)
      ((_ (value . (of . (t . ())))) #f)
      ((_ (value . (of . (t . ()))) (else . (body . ())))
       #'body)
      ((_ (value . (of . (t . ()))) ((opt-id (xs ...)) . (body . ())) tail)
       #'(if (equal? (car value) 'opt-id)
	     (capture-list-f '(xs ...) (caddr value) 'body)
	     (match (value . (of . (t . ()))) tail)))
      ((_ (value . (of . (t . ()))) ((opt-id . (opt-var . ())) . (body . ())) tail)
       #'(if (equal? (car value) 'opt-id)
	     (let ((opt-var (caddr value)))
	       body)
	     (match (value . (of . (t . ()))) tail))))))

(define t (type (Num number)
		(Str string)
		(Seq list)
		(Char char)
		(Fun procedure)))

(define int (instance (Num 5) of t))
;; matching on integers!
(match (int of t)
  ((Num n) (display (format "Got the number ~A!~%" n)))
  (else (display "No value matched!\n")))

(define l (instance (Seq '(1 2 3)) of t))
;; MATCHING ON LISTS!!!!!!!
(match (l of t)
  ((Seq (a b c)) (display (format "Got the triple (~A, ~A, ~A)~%" a b c)))
  (else (display "No value matched!\n")))
