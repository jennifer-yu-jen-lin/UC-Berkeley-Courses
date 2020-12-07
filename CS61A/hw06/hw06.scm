;;;;;;;;;;;;;;;
;; Questions ;;
;;;;;;;;;;;;;;;

; Scheme

(define (cddr s)
  (cdr (cdr s))
)

(define (cadr s)
  ; 'YOUR-CODE-HERE
  (car (cdr s))
)

(define (caddr s)
  ; 'YOUR-CODE-HERE
  (car (cdr (cdr s)))
)

(define (sign x)
  ; 'YOUR-CODE-HERE
  ;  x and returns -1 if x is negative, 0 if x is zero, and 1 if x is positive.
  (cond
      ((< x 0) -1)
      ((= x 0) 0)
      ((> x 0) 1)
      )
)

(define (square x) (* x x))

(define (pow b n)
  ; 'YOUR-CODE-HERE
  (cond
      ((= n 1) b)
      ((even? n) (square (pow b (/ n 2))))
      ((odd? n) (* b (square (pow b (/ (- n 1) 2)))))
      )
)

(define (unique s)
  ; 'YOUR-CODE-HERE
  (cond
      ((null? s) Nil)
      (else (cons (car s) (unique (filter (lambda (x) (not (equal? x (car s)))) (cdr s)))))
      )
)