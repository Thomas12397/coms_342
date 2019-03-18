#lang racket
(require "program.rkt")
(provide (all-defined-out))

;Author: Thomas Haddy 3/17/19

;Problem: 1)

(define (isListSizeN Expr N)
  (and (list? Expr)
       (equal? (length Expr) N)))

(define (isSSeq Expr)
  (and (not (empty? Expr))
       (if (isListSizeN Expr 1)
           ;True
           (isStatement (car Expr))
           ;False
           (and (isStatement (car Expr))
                (isSSeq (rest Expr))))))

(define (isStatement Expr)
  (or (isDecl Expr)
      (isAssign Expr)
      (isIf Expr)))

(define (isDecl Expr)
  (and (isListSizeN Expr 2)
       (equal? (car Expr) 'decl)
       (isVar (cadr Expr))))

(define (isAssign Expr)
  (and (isListSizeN Expr 3)
       (equal? (car Expr) 'assign)
       (isVar (cadr Expr))
       (isArithExpr (caddr Expr))))

(define (isIf Expr)
  (and (isListSizeN Expr 3)
       (equal? (car Expr) 'if)
       (isCondExpr (cadr Expr))
       (isSSeq (caddr Expr))))

(define (isArithExpr Expr)
  (or (and (isListSizeN Expr 3)
           (isOp (car Expr))
           (isArithExpr (cadr Expr))
           (isArithExpr (caddr Expr)))
      (number? Expr)
      (isVar Expr)))

(define (isOp Expr)
  (or (equal? Expr '+)
      (equal? Expr '-)
      (equal? Expr '/)
      (equal? Expr '*)))

(define (isBCond Expr)
  (and (isListSizeN Expr 3)
       (or (equal? (car Expr) 'gt)
           (equal? (car Expr) 'lt)
           (equal? (car Expr) 'eq))
       (isArithExpr (cadr Expr))
       (isArithExpr (caddr Expr))))

(define (isCondExpr Expr)
  (or (and (isListSizeN Expr 3)
           (or (equal? (car Expr) 'or)
               (equal? (car Expr) 'and))
           (isCondExpr (cadr Expr))
           (isCondExpr (caddr Expr)))
      (and (isListSizeN Expr 2)
           (equal? (car Expr) 'not)
           (isCondExpr (cadr Expr)))
      (isBCond Expr)))

(define (isVar Expr)
  (symbol? Expr))

(define synchk isSSeq)

;(synchk program1)
;(synchk program2)

;Problem: 2)

(define (getSymbol Env Sym)
  (if (equal? Sym (caar Env))
      (cadar Env)
      (getSymbol (cdr Env) Sym)))

(getSymbol '((x 1) (y 5)) 'y)
 
  