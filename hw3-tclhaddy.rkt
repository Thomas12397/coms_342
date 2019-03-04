;; Author: Thomas Haddy 3/3/19
;; Homework 3: Racket

#lang racket
(provide (all-defined-out)) ;; for us to test
(require racket/trace)      ;; in case you want to use tracing
(require "DB.rkt")          ;; to access the database definitions
                            ;; in DB.rkt for Q2

(define ptlist '((1 0) (2 3) (4 10) (2 1) (6 5) (5 4) (3 4)
(20 1) (25 3) (30 0) (22 10) (21 10) (20 5)
(27 2) (28 9)))

;; 1(a)
(define (square x)
  (* x x))

(define (get-x x)
  (car x))

(define (get-y y)
  (cadr y))

(define (distance p1 p2)
  (sqrt (+ (square (- (get-x p2) (get-x p1))) (square (- (get-y p2) (get-y p1))))))

(define (ptcomparator p1 p2)
  (< (distance p1 '(0 0)) (distance p2 '(0 0))))

;;(ptcomparator '(5 9) '(1 2))
;;(ptcomparator '(1 2) '(5 9))

;; 1(b)
(define (sort-pair p comp)
  (if (< (length p) 2)
      p
      (list (if (comp (car p) (cadr p))
                (car p)
                (cadr p))
            (if (comp (car p) (cadr p))
                (cadr p)
                (car p)))))

(define (sort-iter l0 l1 comp)
  (cond
    [(null? l0) l1]
    [(null? l1) l0]
    [else (if (comp (car l0) (car l1))
              (cons (car l0) (sort-iter (cdr l0) l1 comp))
              (cons (car l1) (sort-iter (cdr l1) l0 comp)))]))

(define (middle-index l) 
  (exact-floor (/ (length l) 2)))

(define (drop l n)
  (if (zero? n)
      l
      (drop (cdr l) (- n 1))))

(define (drop-last l n)
  (reverse (drop (reverse l) n)))

(define (ptsort l comp)
  (if (<= (length l) 2)
      (sort-pair l comp)
      (sort-iter (sort (drop-last l (- (length l) (middle-index l))) comp)
                 (sort (drop l (middle-index l)) comp) comp)))

;; 1(c)
(define (reduce f l)
  (if (< (length l) 2)
      (car l)
      (f (car l) (reduce f (cdr l)))))

(define (sum l)
  (reduce + l))

(define (map f l)
  (if (null? l)
      l
      (cons (f (car l)) (map f (cdr l)))))

(define (avg l)
  (/ (reduce + l) (length l)))

(define (getcen1 l)
  (list (avg (map car l)) (avg (map cadr l))))

;;(getcen1 ptlist)
;;(getcen1 '((0 0)(3 0)(0 3)))

;; 1(d)
(define (index l n)
  (car (drop l n)))

(define (dec n)
  (- n 1))

(define (median-p-odd l)
  (index l (middle-index l)))

(define (avg-p p0 p1)
  (list (avg (list (get-x p0) (get-x p1))) (avg (list (get-y p0) (get-y p1)))))

(define (median-p-even l)
  (avg-p (index l (dec (middle-index l))) (index l (middle-index l))))

(define (median-p l)
  (if (even? (length l))
      (median-p-even l)
      (median-p-odd l)))

(define (getcen2 l)
  (median-p (sort l ptcomparator)))

;; 1(e)
(define (random-index l)
  (exact-floor (* (length l) (random))))

(define (distinct-rand-indices l)
  (let ([i0 (random-index l)]
        [i1 (random-index l)])
    (if (equal? i0 i1)
        (distinct-rand-indices l)
        (list i0 i1))))

(define (pick l)
  (map (lambda (i) (index l i)) (distinct-rand-indices l)))

;; 1(f)
(define (filter f l)
  (if (null? l)
      l
      (if (f (car l))
          (cons (car l) (filter f (cdr l)))
          (filter f (cdr l)))))

(define (partial2 f x y)
  (lambda (z) (f x y z)))

(define (closer p0 p1 p)
  (< (distance p p0) (distance p p1)))

(define (classify l p0 p1 flag)
  (let ([strat (if flag getcen1 getcen2)])
    (list (strat (sort (filter (partial2 closer p0 p1) l) ptcomparator))
          (strat (sort (filter (partial2 closer p1 p0) l) ptcomparator)))))

;; 1(g)
(define (kclassify l p0 p1 flag k)
  (if (zero? k)
      (list p0 p1)
      (let ([newp (classify l p0 p1 flag)])
        (kclassify l (get-x newp) (get-y newp) flag (dec k)))))

;; 1(h)
(define (nclassify l flag k)
  (let ([p (pick l)])
    (kclassify l (get-x p) (get-y p) flag k)))

;; 2(a)
(define (applyonstds table)
  (lambda (f)
    (map f table)))

;; 2(b)
(define (numberofcourses student)
  (list (car student)
        (cadr student)
        (length (caddr student))))

;;((applyonstds s-table) numberofcourses)

;; 2(c)
(define (gpa-value grade)
  (cond [(equal? grade 'A) 4]
        [(equal? grade 'B) 3]
        [(equal? grade 'C) 2]
        [(equal? grade 'D) 1]
        [(equal? grade 'F) 0]))

(define (get-gpa student g-table)
  (exact->inexact
   (avg (map gpa-value
             (map caddr
                  (filter (lambda (grade)
                            (equal? (cadr grade)
                                    (car student)))
                          g-table))))))

(define (studentgpa g-table)
  (lambda (student)
    (list (car student)
          (cadr student) 
          (get-gpa student g-table))))

;;((applyonstds s-table) (studentgpa g-table))