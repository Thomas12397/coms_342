#lang racket
;(require "program.rkt")
(provide (all-defined-out))

(define (isSSeq Expr)
  (or (equal?) (car Expr) (Statement)
      (equal?) (car Expr) (cadr SSeq)))

(define (Decl)
  )

(define (isOp Expr)
  (or (equal? (car Expr) "+")
      (equal? (car Expr) "-")
      (equal? (car Expr) "/")
      (equal? (car Expr) "*")))
  