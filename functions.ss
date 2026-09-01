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

(define-syntax check-types
  (syntax-rules () ((_ (t x)...) (cons (t x) ...))))

(define-syntax define
  (syntax-rules ()
    ((_ (ident (t x)...) body) (impl-define (ident x ...) (check-types (t x)...) body))))

(import (rename (rnrs base) (define impl-define)))


;; an example
(define (f (integer? x) (string? y))
  (+ x (string->number y)))

(display (f 5 "6"))

