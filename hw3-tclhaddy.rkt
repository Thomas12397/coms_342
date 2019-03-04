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

;; 1A)
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

(ptcomparator '(5 9) '(1 2))
(ptcomparator '(1 2) '(5 9))

;;1B)

;;1C)
(define (reduce f pts)
  (if (< (length pts) 2)
      (car pts)
      (f (car pts) (reduce f (cdr pts)))))

(define (sum pts)
  (reduce + pts))

(define (map f pts)
  (if (null? pts)
      pts
      (cons (f (car pts)) (map f (cdr pts)))))

(define (avg pts)
  (/ (reduce + pts) (length pts)))

(define (getcen1 pts)
  (list (avg (map car pts)) (avg (map cadr pts))))

(getcen1 ptlist)
(getcen1 '((0 0)(3 0)(0 3))) ;;Expected (1 1)

;;1D)

;;1E)

;;1F)

;;1G)

;;1H)

;;2A)

;;2B)

;;2C)

