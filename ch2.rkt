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

(define (scale-list-orig items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list-orig (cdr items) factor))))

#|

(scale-list-orig '(1 2 3) 2)
(cons (* 1 2) (scale-list-orig (2 3) 2))
(cons 2 (scale-list-orig (2 3) 2))
(cons 2 (cons (* 2 2) (scale-list-orig (3) 2)))
(cons 2 (cons 4 (scale-list-orig (3) 2)))
(cons 2 (cons 4 (cons (* 3 2) (scale-list-orig '() 2))))
(cons 2 (cons 4 (cons 6 '()) ; nil returns '()
(cons 2 (cons 4 '(6)))
(cons 2 '(4 6))
'(2 4 6)

|#

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;; Exercise 2.21

;redefine square from chap 1
(define (square x) (* x x))

(define (square-list-one items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-one (cdr items)))))

(define (square-list-two items)
  (map square items))

;;;;;;;;;;;;;;;;

;; Exercise 2.22

; In the first answer, because cons prepends, it is going to go in
; reverse order. The second answer is an attempt to flip it to an
; append operation, but this doesn't work either because it appends
; a list of an integer instead of an integer.

;;;;;;;;;;;;;;;;

;; Exercise 2.23

(define (for-each proc xs)
  (cond ((not (null? xs))
         (proc (car xs))
         (for-each proc (cdr xs)))))

;;;;;;;;;;;;;;;;

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.24
#|

'(1 (2 (3 4)))

      
      \
     / \
    /  /\
   /  /  |
  /  /   |
 /  /   / \
1  2   3   4

|#

;;;;;;;;;;;;;;;;

;; Exercise 2.25

(define q (list 1 3 (list 5 7) 9))
(define r (list (list 7)))
(define s '(1 (2 (3 (4 (5 (6 7)))))) )

; (car (cdr (car (cdr (cdr q)))))
; (car (car r))
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr s))))))))))))

;;;;;;;;;;;;;;;;

;; Exercise 2.26

(define t (list 1 2 3))
(define u (list 4 5 6))

; (append t u) => (list 1 2 3 4 5 6)
; (cons t u) => (list (list 1 2 3) 4 5 6))
; (list t u) => (list (list 1 2 3) (list 4 5 6))

;;;;;;;;;;;;;;;;

;; Exercise 2.27

(define v
  (list (list (list 1 2)) (list 3 4)))

(define (deep-reverse xs)
  (if (pair? xs)
      (map deep-reverse (reverse xs))
      xs))

;;;;;;;;;;;;;;;;



;; Exercise 2.28

;skipped

;;;;;;;;;;;;;;;;


;; Exercise 2.29

;0:

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure)) ;length must be number, structure may also be a mobile

;1:

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;2:

;; THIS DOES NOT WORK -- treats lengths as weights. TODO
(define (total-weight x)
  (cond ((null? x) 0)
        ((not (pair? x)) x)
        (else (+ (total-weight (left-branch x))
                 (total-weight (right-branch x))))))

;3: skipped



;4:

; if we changed the representation of mobiles so that the constructors
; used cons instead of list, the main change is that the right branch
; and the structure are now lists, so we have to drop the cars from our
; selectors:


; (define (right-branch mobile)
;   (cdr mobile))

; (define (branch-structure branch)
;   (car (cdr branch)))

;;;;;;;;;;;;;;;;

(define (scale-tree-orig tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree-orig (car tree)
                                factor)
               (scale-tree-orig (cdr tree)
                                factor)))))



(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree) ; no use of cdr, so no need for null? now
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

 (define atree (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; Exercise 2.30

;define square-tree directly:
(define (square-tree-direct tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

;define square-tree in terms of map and recursion:
(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

;;;;;;;;;;;;;;;;

;; Exercise 2.31

(define (square-tree tree)
  (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;;;;;;;;;;;;;;;;

;; Exercise 2.32

; skipped

;;;;;;;;;;;;;;;;

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (sum-odd-squares-signal tree)
  (accumulate
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (even-fibs n)
  (accumulate 
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square
        (map fib
             (enumerate-interval 0 n)))))

(define
  (product-of-squares-of-odd-elements
   sequence)
  (accumulate
   *
   1
   (map square (filter odd? sequence))))

;; Exercise 2.33

(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length2 sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

;;;;;;;;;;;;;;;;

;; Exercise 2.34

; skipped

;;;;;;;;;;;;;;;;

;; Exercise 2.35

(define (count-leaves-a t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;;;;;;;;;;;;;;;;

;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;;;;;;;;;;;;;;;

;; Exercise 2.37

; skipped

;;;;;;;;;;;;;;;;

;; Exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; (fold-right / 1 (list 1 2 3)) => 1 1/2
; (fold-left / 1 (list 1 2 3)) => 1/6
; (fold-right list nil (list 1 2 3)) => (list (list) (list 1 2 3))
; (fold-left list nil (list 1 2 3)) => (list ( list (list (list) 1) 2) 3)

; Two properties need to be satisfied: associativity and commutativity. E.g.,
; addition and multiplication satisfy both, but division and subtraction do not.
          
;;;;;;;;;;;;;;;;
