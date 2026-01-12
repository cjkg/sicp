#lang sicp

; from 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; from chap 1
(define (average x y) (/ (+ x y) 2))

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

;; Exercise 2.2

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment a b) (cons a b))
(define (start-segment ab) (car ab))
(define (end-segment ab) (cdr ab))

(define (midpoint-segment ab)
  (let ((a (start-segment ab))
        (b (end-segment ab)))
    (make-point (average (x-point a) (x-point b))
                (average (y-point a) (y-point b)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

; (define point-a (make-point 0 5))
; (define point-b (make-point -3 3))
; (define segment-ab (make-segment point-a point-b))
; (print-point (midpoint-segment segment-ab))

;;;;;;;;;;;;;;;



;; Exercise 2.3

#|
(define (perimeter r)
  (* 2 (+ (height r) (width r))))

(define (area r)
  (* (height r) (width r)))

(define (make-rect nw-point se-point)
  (cons nw-point se-point))

(define (get-nw-point r)
  (car r))

(define (get-se-point r)
  (cdr r))

(define (width r)
  (let ((a (get-nw-point r))
        (b (get-se-point r)))
    (- (x-point b) (x-point a))))

(define (height r)
  (let ((a (get-nw-point r))
        (b (get-se-point r)))
    (- (y-point a) (y-point b))))
    
(define nw (make-point 0 5))
(define se (make-point 5 0))

(define rectangle-a (make-rect nw se))

(perimeter rectangle-a)
(area rectangle-a)


;-------------;
; nw corner plus height plus width
(define (make-rect-alt x y w h)
  (cons (make-point x y) (cons w h)))

(define (width alt-r)
  (car (cdr alt-r)))

(define (height alt-r)
  (cdr (cdr alt-r)))

(define rectangle-b (make-rect-alt 0 5 5 5))

(perimeter rectangle-b)
(area rectangle-b)
  
|#


;;;;;;;;;;;;;;;


(define (cons-alt x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (error "argument not 0 or 1: CONS" m))))
  dispatch)

(define (car-alt z) (z 0))
(define (cdr-alt z) (z 1))

;; Exercise 2.4

(define (cons-alt2 x y)
  (lambda (m) (m x y)))

(define (car-alt2 z)
  (z (lambda (p q) p)))

(define (cdr-alt2 z)
  (z (lambda (p q) q)))

;;;;;;;;;;;;;;;

;; Exercise 2.5

; skipped

;;;;;;;;;;;;;;;


;; Exercise 2.6

(define church-zero (lambda (f) (lambda (x) x)))

(define (church-inc n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define church-one (lambda (f) (lambda (x) f x)))

(define church-two (lambda (f) (lambda (x) (f (f x)))))

; skipped addition for now

;;;;;;;;;;;;;;;

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (lower-bound y) 0 (upper-bound y))
      (error "can't divide by an interval that spans 0") ;Ex. 2.10
      (mul-interval x
                    (make-interval
                     (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

;; Exercise 2.7

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;;;;;;;;;;;;;;;

;; Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (lower-bound y))
                 (- (upper-bound x)
                    (upper-bound y))))

;;;;;;;;;;;;;;;

;; Exercise 2.9
    
; skipped

;;;;;;;;;;;;;;;

;; Exercise 2.10

; See division function above

;;;;;;;;;;;;;;;;

;; Exercise 2.11

; skipped

;;;;;;;;;;;;;;;;

(define (make-center-width c w)
   (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

;; Exercise 2.12

(define (make-center-percent c p)
  (make-interval (- c p)
                 (+ c p)))

(define (percent i)
  (/ (- (upper-bound i)
        (lower-bound i)) (* 2 (center i))))

;;;;;;;;;;;;;;;;


;; Exercise 2.13

; skipped

;;;;;;;;;;;;;;;;

;; Exercise 2.14

; skipped

;;;;;;;;;;;;;;;;

;; Exercise 2.15

; skipped

;;;;;;;;;;;;;;;;

;; Exercise 2.16

; skipped

;;;;;;;;;;;;;;;;

;; Exercise 2.17

(define (last-pair l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

;;;;;;;;;;;;;;;;

;; Exercise 2.18

(define (reverse l)
  (define (iter l l2)
    (if (null? l)
        l2
        (iter (cdr l) (cons (car l) l2))))
  (iter l '()))

;;;;;;;;;;;;;;;;

;; Exercise 2.19

; skipped

;;;;;;;;;;;;;;;;


;; Exercise 2.20

(define a (list 1 2 3 4 5))

(define (same-parity l)
  (let ((parity (even? (car l))))
    (define (iter x y)
      (if (null? x)
          y
          (iter (cdr x) (if (eq? parity (even? (car x)))
                            (cons (car x) y)
                            y))))
    (reverse (iter l '())))) ; TODO how to cdr down a list without extra reverse?

;;;;;;;;;;;;;;;;