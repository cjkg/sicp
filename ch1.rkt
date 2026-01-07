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

(define (fast-expt-i b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))


#|
(expt 2 4 1)
(expt (square 2) 2 1)
(expt 4 2 1)
(expt 16 1 1)
(expt 16 0 16)
16
|#
;;;;;;;;;;;;;;;;


;; Exercise 1.17

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
;;;;;;;;;;;;;;;;

;; Exercise 1.18

(define (fast-mult-i x y)
  (define (fast-mult-iter x y a)
    (cond ((= y 0) a)
          ((even? y) (fast-mult-iter (double x) (halve y) a))
          (else (fast-mult-iter x (- y 1) (+ a x)))))
  (fast-mult-iter x y 0))

#|

(fast-mult-i 3 4)
(fast-mult-iter 3 4 0)
(fast-mult-iter (double 3) (halve 4) 0)
(fast-mult-iter 6 2 0)
(fast-mult-iter (double 6) (halve 2) 0)
(fast-mult-iter 12 1 0)
(fast-mult-iter 12 0 12)
12

|#

;;;;;;;;;;;;;;;;

;; Exercise 1.19

; skipped

;;;;;;;;;;;;;;;;


(define (gcd-mine a b)
  (cond ((> b a) (gcd-mine b a))
        ((= b 0) a)
        (else (gcd-mine b (remainder a b)))))


;; Exercise 1.20

#|
applicative order
-----------------
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2

normal order
------------
skipped

|#
;;;;;;;;;;;;;;;;

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next x)
  (if (< x 3)
      (+ x 1)
      (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (halve exp) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))


;; Exercise 1.21

; (smallest-divisor 199) -> 199
; (smallest-divisor 1999) -> 1999
; (smallest-divisor 19999) -> 7

;;;;;;;;;;;;;;;;;

;; Exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes m n)
  (cond ((> m n) (display "\nDone "))
      (else
            (timed-prime-test m)
            (search-for-primes (+ m 1) n))))

; (search-for-primes 1000 2000) => 1009, 1013, 1019
; (search-for-primes 10000 11000) => 10007, 10009, 10037
; (search-for-primes 100000 101000) => 100003, 100019, 100043
; (search-for-primes 1000000 1001000) => 1000003, 1000033, 1000037

; Θ(sqrt(n)) is roughly accurate 

;;;;;;;;;;;;;;;;

;; Exercise 1.23

; Updated functions above. As others have noted the overhead of calling
; next seems to actually slow down the functions called in search-for-primes

;;;;;;;;;;;;;;;;



;; Exercise 1.24

; I'd expect the millions range to take just slightly longer to compute
; than the thousands, and that was the case, just a millisecond more in
; most cases.

;;;;;;;;;;;;;;;;



;; Exercise 1.25

; skipped

;;;;;;;;;;;;;;;;


;; Exercise 1.26

; (square n) evaluates n once and substitutes n for the second term in
; the (* n n) call of square. Louis's evaluates n twice needlessly. This
; decreases the speed of the function.

;;;;;;;;;;;;;;;;

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

#|
(sum-integers 1 3)
(+ 1 (sum-integers 2 3))
(+ 1 (+ 2 (sum-integers 3 3)))
(+ 1 (+ 2 (+ 3 (sum-integers 4 3))))
(+ 1 (+ 2 (+ 3 0)))
(+ 1 (+ 2 3))
(+ 1 5)
6
|#

(define (sum-integers-i a b)
  (define (iter a b c)
    (if (> a b)
        c
        (iter (+ 1 a) b (+ c a))))
    (iter a b 0))

#|
(iter 1 3 0)
(iter (+ 1 1) 3 (+ 0 1))
(iter 2 3 1)
(iter (+ 1 2) 3 (+ 1 2))
(iter 3 3 3)
(iter (+ 1 3) 3 (+ 3 3))
(iter 4 3 6)
6
|#

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

#|
(sum-cubes 1 3)
(+ (cube 1) (sum-cubes (+ 1 1) 3))
(+ 1 (sum-cubes 2 3))
(+ 1 (+ (cube 2) (sum-cubes (+ 2 1) 3)))
(+ 1 (+ 8 (sum-cubes 3 3)))
(+ 1 (+ 8 (+ (cube 3) (sum-cubes 4 3))))
(+ 1 (+ 8 (+ 27 0)))
(+ 1 (+ 8 27))
(+ 1 35)
36
|#

(define (sum-cubes-i a b)
  (define (iter a b c)
    (if (> a b)
        c
        (iter (+ a 1) b (+ c (cube a)))))
  (iter a b 0))

#|
(sum-cubes-i 1 3)
(iter 1 3 0)
(iter (+ 1 1) 3 (+ 0 (cube 1)))
(iter 2 3 1)
(iter (+ 2 1) 3 (+ 1 8))
(iter 3 3 9)
(iter (+ 3 1) 3 (+ 9 (cube 3)))
(iter 4 3 36)
36
|#

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) 
         (pi-sum (+ a 4) b))))
#|
(define (<name> a b)
  (if (> a b)
      0
      (+ (<term> a)
         (<name> (<next> a) b))))
|#

#|
(define (<name> a b)
  (iter (iter a b c)
        (if (> a b)
            c
            (iter (<next> a) b (+ c (<term> a))
|#

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes-2 a b)
  (sum cube a inc b))

#|
(sum-cubes-2 1 3)
(sum cube 1 inc 3)
(+ (cube 1) (sum cube (inc 1) inc 3)
(+ 1 (sum cube 2 inc 3))
(+ 1 (+ (cube 2) (sum cube (inc 2) inc 3)))
(+ 1 (+ 8 (sum cube 3 inc 3)))
(+ 1 (+ 8 (+ (cube 3) (sum cube (inc 3) inc 3))))
(+ 1 (+ 8 (+ 27 (sum cube 4 inc 3))))
(+ 1 (+ 8 (+ 27 0)))
(+ 1 (+ 8 27))
(+ 1 35)
36
|#

(define (identity x) x)

(define (sum-integers-2 a b)
  (sum identity a inc b))

#|
(sum-integers-2 1 3)
(sum identity 1 inc 3)
(+ (identity 1) (sum identity (inc 1) inc 3))
(+ 1 (sum identity 2 inc 3))
(+ 1 (+ (identity 2) (sum identity (inc 2) inc 3)))
(+ 1 (+ 2 (sum identity 3 inc 3)))
(+ 1 (+ 2 (+ (identity 3) (sum identity (inc 3) inc 3))))
(+ 1 (+ 2 (+ 3 (sum identity 4 inc 3))))
(+ 1 (+ 2 (+ 3 0)))
(+ 1 (+ 2 3))
(+ 1 5)
6
|#

(define (pi-sum-2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;; Exercise 1.29

; skipped

;;;;;;;;;;;;;;;;

;; Exercise 1.30

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;;;;;;;;;;;;;;;

;; Exercise 1.31

; some ideas...

(define (product-cubes a b)
  (if (> a b)
      1
      (* (cube a) (product-cubes (+ a 1) b))))

(define (product-cubes-i a b)
  (define (iter a c)
    (if (> a b)
        c
        (iter (+ a 1) (* (cube a) c))))
  (iter 1 1)
  )

; part 1 (recursive)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-cubes-new a b)
  (product cube a inc b))

(define (product-factorial n)
  (product identity 1 inc n))

; skipped the pi approx part

; part 2 (iterative)

(define (product-i term a next b)
  (define (iter a c)
    (if (> a b)
        c
        (iter (inc a) (* c (term a)))))
  (iter a 1))

;;;;;;;;;;;;;;;;

;; Exercise 1.32

; Part 1 - recursive

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (product-factorial-accumulate n)
  (accumulate * 1 identity 1 inc n))

(define (sum-cubes-accumulate a b)
  (accumulate + 0 cube a inc b))

(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))

(define (product-accumulate term a next b)
  (accumulate * 1 term a next b))

; Part 2 - iterative

(define (accumulate-i combiner null-value term a next b)
  (define (iter a c)
    (if (> a b)
        c
        (iter (next a) (combiner (term a) c))))
  (iter a null-value))

(define (product-factorial-accumulate-i n)
  (accumulate-i * 1 identity 1 inc n))

(define (sum-cubes-accumulate-i a b)
  (accumulate-i + 0 cube a inc b))

(define (sum-accumulate-i term a next b)
  (accumulate-i + 0 term a next b))

(define (product-accumulate-i term a next b)
  (accumulate-i * 1 term a next b))

;;;;;;;;;;;;;;;;

;; Exercise 1.33

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b
                                     filter))))

; express the sum of the squares of the prime numbers
; in the interval a to b:

(define (sum-of-squared-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

; express the product of all the positive integers less
; than n that are relatively prime to n (i.e., all positive
; integers i < n such that GCD ( i , n ) = 1 ):

(define (product-relative-primes n)
  (define (is-relative-prime-to? m)
    (= (gcd m n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) is-relative-prime-to?))

;;;;;;;;;;;;;;;;

(define (pi-sum-lambda a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda(x) (+ x 4))
       b))

(define (integral-lambda f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; named
(define (f-1 x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

; transitional
(define (f-2 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; let
(define (f-3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; Exercise 1.34

; (define (f g) (g 2))
; (f square) => 4
; (f (lambda (z) (* z (+ z 1)))) => 6
;
; (f f) fails because 2 is not a procedure

;;;;;;;;;;;;;;;;



  
; half-interval method
#|

simple but powerful method for finding roots of an equation
f(x) = 0, where f is a continuous function.

If we are given points a and b such that f(a) < 0 f(b), then f
must have at least one zero betweeen a and b.

To locate a zero, let x be the average of a and b, and compute
f(x).

If f(x) > 0, then f must have a zero between a and x.
If f(x) < 0, then f must have a zero between x and b.

Continuing in this way, we can identify smaller and smaller
intervals on which f must have a zero.

When we reach a point where the interval is small enough, the
process stops.

|#

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
            ((positive? test-value)
             (search f neg-point midpoint))
            ((negative? test-value)
             (search f midpoint pos-point))
            (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; bad fixed-point sqrt (doesn't converge)

; (define (sqrt x)
;   (fixed-point (lambda (y) (/ x y))
;                1.0))

(define (sqrt-fixed-point x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))
