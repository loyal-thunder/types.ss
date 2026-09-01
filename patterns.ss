;; PATTERNS.SS

;; a very minimal pattern matching macro implemented on top of R6RS.

(define-syntax match
  (lambda (x)
    (syntax-case x (else)
      ((_ value) #f) ;; base case: no else-branch found
      ((_ value (else body...)) #'(begin body...)) ;; base case: else-branch found

      ;; predicate without a pattern variable to bind.
      ((_ value ((p?) body...) . tail...)
       #'(if (p? value) (begin body...) (match . (value . tail...))))

      ;; predicate with a pattern list to bind.
      ((_ value ((p? (v . vs...)) body...) . tail...)
       #'(let* ((l '(v . vs...)) (len-l (length l)))
	       (if (and (p? value) (p? l) (= (length value) len-l))
		   (apply (lambda (v . vs...) body...) value)
		   (match . (value . tail...)))))

      ;; predicate with a single pattern variable to bind.
      ((_ value ((p? v) body...) . tail...)
       #'(if (p? value)
	     ((lambda (v) body...) value)
	     (match . (value . tail...)))))))


;; END IMPLEMENTATION

;; EXAMPLE USAGE

(define (foo x)
  (match x
    ((null?) 0)
    ((integer? n) 1)
    ((string? s) 2)
    ((list? (a b c)) 3)
    (else 5)))

(foo '()) ;; returns 0
(foo 2) ;; returns 1
(foo "ciao") ;; returns 2;
(foo '(1 2 3)) ;; returns 3

(foo '(1 2)) ;; returns 5
(foo #f) ;; returns 5

;; this displays all the results above on stdout, in order from top to bottom:

(begin
  (display "result: ")
  (for-each
   (lambda (x) (display (format "~a " x)))
   (map foo (list '() 2 "ciao" '(1 2 3) '(1 2) #f)))
  (newline))

