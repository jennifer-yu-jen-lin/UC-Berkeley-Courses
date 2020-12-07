(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  ; replace-this-line
  (define (helper s n current)
    (cond ((null? s) current)
           (else (helper (cdr s)
                         (+ n 1) 
                         (append current (cons (cons n (cons (car s) nil)) nil))
                         ))
    )
  )
  (helper s 0 nil)
)
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  ; 'replace-this-line
  (cond 
    ((null? list1) list2)
    ((null? list2) list1)
    ((comp (car list1) (car list2)) 
          (cons (car list1) (cons (car list2) (merge comp (cdr list1) (cdr list2))))   )
    (else (cons (car list2) (cons (car list1) (merge comp (cdr list1) (cdr list2))))
          )))
  ; END PROBLEM 16


(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Problem 17

(define (nondecreaselist s)
    ; BEGIN PROBLEM 17
    ;'replace-this-line
  (cond ((null? s) nil)
        ((null? (cdr s)) (cons s nil))
        ((> (car s) (cadr s))
           (cons (cons (car s) nil)
                 (nondecreaselist (cdr s))))
        (else
           (cons (cons (car s)
                       (car (nondecreaselist (cdr s))))
                 (cdr (nondecreaselist (cdr s)))))))
    ; END PROBLEM 17

; ((1 2 3 4) (1 2 3 4) (1 1 1 2) (1 1) (0 4) (3) (2) (1))







