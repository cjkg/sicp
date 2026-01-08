#lang sicp

; from 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(define (make-rat n d)
  (define (normalize-signs rat)
    (let ((n (car rat))
          (d (cdr rat)))
    (if (< d 0)
        (make-rat (- 0 n) (abs d))
        rat)))
  (let ((g (gcd n d)))
    (normalize-signs (cons (/ n g)
          (/ d g)))))


; (define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (one-half) (make-rat 1 2))
(define (one-third) (make-rat 1 3))

;; Exercise 2.1

; the first case is covered by gcd, it will make both n and d positive if both
; are negative.

; the second-case is handled in the new definition of normalize-signs




