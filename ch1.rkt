#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))

(define (<= x y)
  (or (< x y) (= x y)))

;; Exercise 1.3

(define (max-two-sum-of-squares x y z)
  (cond
    ((and (>= x y) (>= y z)) (+ (square x) (square y)))
    ((and (>= y x) (>= z x)) (+ (square y) (square z)))
    ((and (>= x y) (>= z y)) (+ (square z) (square x)))))

;;;;;;;;;;;;;;;

(define (average x y) (/ (+ x y) 2.0))

(define (sqrt x)
  (define (good-enough? guess y)
    (< (abs (- (square guess) y)) 0.0001))
  (define (improve guess y)
    (average guess (/ y guess)))
  (define (sqrt-iter guess y)
    (if (good-enough? guess y)
        guess
        (sqrt-iter (improve guess y) y)))
  (sqrt-iter 1.0 x))

;; Exercise 1.6

(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-if-sqrt x)
  (define (good-enough? guess y)
    (< (abs (- (square guess) y)) 0.0001))
  (define (improve guess y)
    (average guess (/ y guess)))
  (define (sqrt-iter guess y)
    (new-if (good-enough? guess y)
        guess
        (sqrt-iter (improve guess y) y)))
  (sqrt-iter 1.0 x))

;infinite recursion as the new-if keeps evaluating sqrt-iter,
;unlike the special form of if

;;;;;;;;;;;;;;



;; Exercise 1.7

(define (new-sqrt x)
  (define (good-enough? old-guess new-guess)
    (< (abs (- old-guess new-guess)) 0.001))
  (define (improve guess y)
    (average guess (/ y guess)))
  (define (sqrt-iter guess old-guess y)
    (if (good-enough? old-guess guess)
        guess
        (sqrt-iter (improve guess y) guess y)))
  (sqrt-iter 1.0 2.0 x))

;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

;;Exercise 1.8

(define (cube-rt x)
  (define (good-enough? guess y)
    (< (abs (- (cube guess) y)) 0.0001))
  (define (improve guess y)
    (/ (+ (/ y (square guess)) (* 2 guess)) 3))
  (define (sqrt-iter guess y)
    (if (good-enough? guess y)
        guess
        (sqrt-iter (improve guess y) y)))
  (sqrt-iter 1.0 x))

;;;;;;;;;;;;;;

(define (factorial-recursive n)
  (if (= n 1)
  1
  (* n (factorial (- n 1)))))

(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product 
        (fact-iter (* product counter)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))


;; Exercise 1.9

(define (plus-a a b)
  (if (= a 0)
      b
      (inc (plus-a (dec a) b))))

(define (plus-b a b)
  (if (a = 0)
      b
      ((dec a) (inc b))))

#|
recursive:
(plus-a 3 4)
(inc (plus-a 2 4))
(inc (inc (plus-a 1 4)))
(inc (inc (inc 4)))
(inc (inc 5))
(inc 6)
7

iterative:
(plus-b 3 4)
(plus-b 2 5)
(plus-b 1 6)
(plus-b 0 7)
7
|#

;;;;;;;;;;;;;


;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
; doubles n
(define (g n) (A 1 n))
; 2^n for n > 1
(define (h n) (A 2 n))
; tetration

#|
EXAMPLES FOR SMALL VALUES:
(h 1)
(A 2 1)
2

(h 2)
(A 2 2)
(A (- 2 1) (A 2 (- 2 1)))
(A 1 (A 2 1))
(A 1 2)
(A (- 1 1) (A 1 (- 2 1)))
(A 0 (A 1 1))
(* 2 (A 1 1))
(* 2 2)
4

(h 3)
(A 2 3)
(A 1 (A 2 2))
(A 1 (A 1 (A 2 1)))
(A 1 (A 1 2))
(A 1 (A 0 (A 1 1)))
(A 1 (A 0 2))
(A 1 (* 2 2))
(A 1 4)
(A 0 (A 1 3))
(A 0 (A 0 (A 1 2)))
(A 0 (A 0 (A 0 (A 1 1))))
(A 0 (A 0 (A 0 2)))
(A 0 (A 0 (* 2 2)))
(A 0 (* 2 4))
(A 0 8)
(* 2 8)
16
|#
;;;;;;;;;;;;;;;;

(define (fib-r n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-r (- n 1))
                 (fib-r (- n 2))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  fib-iter 1 0 n)

#|
Example:

(fib 5)
(fib-iter 1 0 5)
(fib-iter 1 1 4)
(fib-iter 2 1 3)
(fib-iter 3 2 2)
(fib-iter 5 3 1)
(fib-iter 8 5 0)
5
|#

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0)
               (= kinds-of-coins 0))
           0)
          (else
           (+ (cc amount (- kinds-of-coins 1))
              (cc (- amount (first-denomination kinds-of-coins))
                  kinds-of-coins)))))
  (cc amount 5))

