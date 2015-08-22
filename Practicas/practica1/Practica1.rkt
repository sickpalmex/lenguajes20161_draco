#lang plai

; ========== FUNCIONES AUXILIARES ==========

; Calcula la longitud de una lista
(define (long lst)
  (if (empty? lst)
    0
    (+ 1 (long (cdr lst)))))

; Suma los elementos de una lista
(define (sum-lst lst)
  (if (empty? lst)
    0
    (+ (first lst) (sum-lst (cdr lst)))))

; Dadas dos listas, devuelve una lista de dos elementos con la cabeza de cada una de las listas recibidas
(define (concat-head a-lst b-lst)
  (if (or (empty? a-lst)(empty? b-lst))
    '()
    (list (first a-lst)(first b-lst))))

; ========== PRACTICA 1 ==========

; EJERCICIO 1: Toma dos números enteros positivos z y w y regresa el numero que se obtiene de elevar el numero z a la potencia w.

(define (pow z w)
  (cond
    [(= w 0) 1]
    [(= w 1) z]
    [else (* (* z z) (pow z (- w 2)))]))

; EJERCICIO 2: Dado una lista no vacía de números, regresar el promedio de esta

(define (average a-lst)
  (if (empty? a-lst)
    0
    (/ (sum-lst a-lst) (long a-lst))))

; EJERCICIO 3: Dado un numero entero positivo, regresa una lista con los números primos contenidos entre 2 y el numero entero dado

; EJERCICIO 4: zip

(define (zip a-lst b-lst)
  (cond
    [(or (empty? a-lst) (empty? b-lst)) '()]
    [else (cons (concat-head a-lst b-lst)(zip (cdr a-lst)(cdr b-lst)))]))

; EJERCICIO 5: reduce - Dada una función de aridad 2 y una lista de n elementos, regresar la evaluación de la función encadenada de todos los elementos

(define (reduce fn lst)
  (if (empty? (cdr lst))
    (car lst)
    (fn (car lst) (reduce fn (cdr lst)))))

; EJERCICIO 6: Dadas dos listas, regresa la concatenación de la primera con la segunda

(define (mconcat a-lst b-lst)
  (cond
    [(empty? a-lst) b-lst]
    [(empty? b-lst) a-lst]
    [else (cons (car a-lst) (mconcat (cdr  a-lst) b-lst))]))

; EJERCICIO 7: Dada una función de aridad 1 y una lista, regresa una lista con la aplicación de la función a cada uno de los elementos de la lista original

(define (mmap fn lst)
  (if (empty? (cdr lst))
    (list (fn (car lst)))
    (cons (fn (car lst)) (mmap fn (cdr lst)))))

; EJERCICIO 8:

; EJERCICIO 9: Dado un predicado de un argumento y una lista, regresa la lista original sin los elementos que al aplicar el predicado, regresa falso.

(define (any? pred lst)
  (cond
    [(empty? lst) (pred lst)]
    [(pred (car lst)) true]
    [else (any? pred (cdr lst))]))

; EJERCICIO 10: Dado un predicado de un argumento y una lista, regresa solamente #t cuando cada uno de los elementos de la lista regresa #t para el predicado dado. En caso contrario regresa #f

(define (every? pred lst)
  (cond
    [(empty? lst)(pred lst)]
    [(pred (car lst))
     (if (empty? (cdr lst))
       (pred (car lst))
       (every? pred (cdr lst)))]
    [else false]))

; ========== TESTS ==========

; Ejercicio 1

(test (pow 2000 0) 1)
(test (pow 2000 1) 2000)
(test (pow 2 3) 8)
(test (pow 8 6) 262144)
(test (pow 10 5) 100000)

; Ejercicio 2

(test (average '(5)) 5)
(test (average '(3 2 6 2 1 7 2 1)) 3)
(test (average '(10 7 13)) 10)

; Ejercicio 4

(test (zip '(1 2) '(3 4)) '((1 3) (2 4)))
(test (zip '(1 2 3) '()) '())
(test (zip '() '(4 5 6)) '())
(test (zip '(8 9) '(3 2 1 4)) '((8 3) (9 2)))
(test (zip '(8 9 1 2) '(3 4)) '((8 3) (9 4)))

; Ejercicio 5

(test (reduce + '(1 2 3 4 5 6 7 8 9 10)) 55)
(test (reduce zip '((1 2 3) (4 5 6) (7 8 9))) '((1 (4 7)) (2 (5 8)) (3 (6 9))))

; Ejercicio 6

(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(test (mconcat '() '(4 5 6)) '(4 5 6))
(test (mconcat '(1 2 3) '()) '(1 2 3))

; Ejercicio 7

(test (mmap add1 '(1 2 3 4)) '(2 3 4 5))
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))

; Ejercicio 9

(test (any? number? '()) #f)
(test (any? number? '(a b c d 1)) #t)
(test (any? symbol? '(1 2 3 4)) #f)

; Ejercicio 10

(test (every? number? '()) #f)
(test (every? number? '(1 2 3)) #t)
(test (every? number? '(1 2 3 a)) #f)
(test (every? symbol? '(1 2 3 a)) #f)
