#lang plai

(define (pow z w)
  (cond 
    [(= w 0) 1]
    [(= w 1) z]
    [else (* (* z z) (pow z (- w 2)))]))

(define (long lst)
  (if (empty? lst)
    0
    (+ 1 (long (cdr lst)))))

(test (pow 2000 0) 1)
(test (pow 2000 1) 2000)
(test (pow 2 3) 8)
(test (pow 8 6) 262144)
(test (pow 10 5) 100000)

(test (long '()) 0)
(test (long '(1 2 3)) 3)

(define (average a-lst)
  (if (empty? a-lst)
    0
    (/ (+ (first a_lst) (average (cdr a_lst))) (long a-lst))))

(test (average '(5)) 5)
(test (average '(3 2 6 2 1 7 2 1)) 3)
(test (average '((10 7 13)) 10))
