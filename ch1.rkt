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

;; Exercise 1.11

(define (f-r n)
  (if (< n 3)
      n
      (+ (f-r (- n 1))
         (* 2 (f-r (- n 2)))
         (* 3 (f-r (- n 3))))))

#|
(f 1)
1

(f 2)
2

(f 3)
(+ (f 2) (* 2 (f 1)) (* 3 (f 0)))
(+ 2 (* 2 1) (* 3 0))
(+ 2 2 0)
4

(f 4)
(+ (f (- 4 1)) (* 2 (f (- 4 2))) (* 3 (f (- 4 3))))
(+ (f 3) (* 2 (f 2)) (* 3 (f 1)))
(+ (f 3) (* 2 2) (* 3 1))
(+ (f 3) 4 3)
(+ (+ (f 2) (* 2 (f 1)) (* 3 (f 0))) 4 3)
(+ (+ 2 (* 2 1) (* 3 0)) 4 3)
(+ (+ 2 2 0) 4 3))
(+ 4 4 3)
11

(f 5)
(+ (f 4) (* 2 (f 3)) (* 3 (f 2)))
(+ 11 (* 2 4) (* 3 2))
25
|#
        
;;;;;;;;;;;;;;;;


;; Exercise 1.12

(define (pascal column row)
  (if (or (= 0 column) (= row column))
      1
      (+ (pascal
          (- column 1) (- row 1))
         (pascal
          column (- row 1)))))

;;;;;;;;;;;;;;;;

;; Exercise 1.13

;; skipped

;;;;;;;;;;;;;;;;



;; Exercise 1.14

;; skipped

;;;;;;;;;;;;;;;;



;; Exercise 1.15

(define (sine angle)
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; skipped

;;;;;;;;;;;;;;;;


(define (expt-r-naive b n)
  (if (= n 0)
      1
      (* b (expt-r-naive b (- n 1)))))

#|
(expt 2 3)
(* 2 (expt 2 (- 3 1)))
(* 2 (expt 2 2))
(* 2 (* 2 (expt 2 (- 2 1)))
(* 2 (* 2 (expt 2 1))
(* 2 (* 2 (* 2 (expt 2 (- 1 1)))))
(* 2 (* 2 (* 2 (expt 2 0))))
(* 2 (* 2 (* 2 1)))
(* 2 (* 2 2))
(* 2 4)
8
|#





(define (expt-i-naive b counter product)
  (if (= counter 0)
      product
      (expt-i-naive b
                 (- counter 1)
                 (* b product))))

#|
(expt 2 3 1)
(expt 2 (- 3 1) (* 2 1))
(expt 2 2 2)
(expt 2 (- 2 1) (* 2 2))
(expt 2 1 4)
(expt 2 (- 1 1) (* 2 4))
(expt 2 0 8)
8
|#


(define (fast-expt-r b n)
  (cond ((= n 0) 
         1)
        ((even? n) 
         (square (fast-expt-r b (/ n 2))))
        (else 
         (* b (fast-expt-r b (- n 1))))))

#|
(fast-expt 2 3)
(* 2 (fast-expt 2 (- 3 1)))
(* 2 (fast-expt 2 2))
(* 2 (square (fast-expt (2 (/ 2 2)))))
(* 2 (square (fast-expt (2 1))
(* 2 (square (* 2 (fast-expt (2 0)))))
(* 2 (square (* 2 1)))
(* 2 (square 2))
(* 2 (* 2 2))
(* 2 4)
8
|#


;; Exercise 1.16

#|
(define (fast-expt b n)
  (define (fast-expt-iter b c a n)
    (cond ((= c n)
           a)
          ((even? n)
           (fast-expt-iter 
          
|#
;;;;;;;;;;;;;;;;


;; Excercise 1.17

(define (mult-add a b)
  (if (= b 0)
      0
      (+ a (mult-add a (- b 1)))))

(define (double n) (* n 2))

(define (halve n) (/ n 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
         ((even? a)
          (double (fast-mult b (halve a))))
         (else (+ a (fast-mult a (- b 1))))))

#|
(fast-mult 3 3)
(+ 3 (fast-mult 3 (fast-mult 3 2)))
(+ 3 (+ 3 (fast-mult 3 1)))
(+ 3 (+ 3 (+ 3 (fast-mult 3 0))))
(+ 3 (+ 3 (+ 3 0)))
(+ 3 (+ 3 3))
(+ 3 6)
9

(fast-mult 4 3)
(double (fast-mult 3 (halve 4)))
(double (fast-mult 3 2))
(double (+ 3 (fast-mult 3 (- 2 1))))
(double (+ 3 (fast-mult 3 1)))
(double (+ 3 (+ 3 (fast-mult 3 0))))
(double (+ 3 (+ 3 0))))
(double (+ 3 3))
(double 6)
12

(mult-add 4 3)
(+ 4 (mult-add 4 2))
(+ 4 (+ 4 (mult-add 4 1)))
(+ 4 (+ 4 (+ 4 (mult-add 4 0))))
(+ 4 (+ 4 (+ 4 0)))
(+ 4 (+ 4 4))
(+ 4 8)
12

(fast-mult 4 8)
(double (fast-mult 8 (halve 4)))
(double (fast-mult 8 2))
(double (double (fast-mult 8 1)))
(double (double (+ 8 (fast-mult 0))))
(double (double (+ 8 0)))
(double (double 8))
(double 16)
32

(mult-add 4 8)
(+ 4 (mult-add 4 7))
(+ 4 (+ 4 (mult-add 4 6)))
(+ 4 (+ 4 (+ 4 (mult-add 4 5))))
(+ 4 (+ 4 (+ 4 (+ 4 (mult-add 4 4)))))
(+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (mult-add 4 3))))))
(+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (mult-add 4 2)))))))
(+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (mult-add 4 1))))))))
(+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (mult-add 4 0)))))))))
(+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 0)))))))))
(+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+4 4))))))))
(+ 4 (+ 4 (+ 4 (+ 4 (+ 4 (+ 4 8))))))
(+ 4 (+ 4 (+ 4 (+ 4 (+ 4 12)))))
(+ 4 (+ 4 (+ 4 (+ 4 16))))
(+ 4 (+ 4 (+ 4 20)))
(+ 4 (+ 4 24))
(+ 4 28)
32

|#
;;;;;;;;;;;;;;;;;