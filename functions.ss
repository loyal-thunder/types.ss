;;     DONE
;; a single function definition with a specific type signature
;; example:
;; (define (only-ints (int x) (int y))
;;   (+ x y))

;;     TODO
;; functions with multiple definitions (overloads)
;; example:
;; (define (two-versions)
;;   (((int x) (int y))
;;    (+ x y))
;;   (((string s) (string r))
;;    (string-append s r)))


(define (fail msg)
  (raise (condition (make-error) (make-message-condition msg))))

(define-syntax type-check
  (syntax-rules ()
    ((_ t v) (unless (t v) (fail (format "type mismatch for value ~a\n" v))))))

(define-syntax type-assoc
  (syntax-rules (int string list char bool func float real complex)
    ((_ int x) (type-check number? x))
    ((_ string x) (type-check string? x))
    ((_ list x) (type-check list? x))
    ((_ char x) (type-check char? x))
    ((_ bool x) (type-check boolean? x))
    ((_ func x) (type-check procedure? x))
    ((_ float x) (type-check flonum? x))
    ((_ real x) (type-check real? x))
    ((_ complex x) (type-check complex? x))))

(define-syntax check-types
  (syntax-rules () ((_ (t x)...) (cons (type-assoc t x) ...))))

(define-syntax define
  (syntax-rules ()
    ((_ (ident (t x)...) body) (impl-define (ident x ...) (check-types (t x)...) body))))

(import (rename (rnrs base) (define impl-define)))


;; an example
(define (f (int x) (string y))
  (+ x (string->number y)))

(display (f 5 "6"))

