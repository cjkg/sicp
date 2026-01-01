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



;;Exercise 1.7

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

;;;;;;;;;;;;;;

;;Exercise 1.8

(define (cube-root x)
  (define (good-enough? old-guess new-guess)
    (< (abs (- old-guess new-guess)) 0.001))
  (define (improve guess z)
    (/ (+ (/ guess (square z)) (* 2 guess)) 3))
  (define (cube-root-iter guess old-guess y)
    (if (good-enough? old-guess guess)
        guess
        (cube-root-iter (improve guess y) guess y)))
  (cube-root-iter 1.0 2.0 x))
;;;;;;;;;;;;;;