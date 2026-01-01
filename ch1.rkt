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
