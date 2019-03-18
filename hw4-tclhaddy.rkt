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

(define (getSymbolValue Env Sym)
  (if (equal? Sym (caar Env))
      (caadr Env)
      (getSymbolValue (cdr Env) Sym)))

(define (updateSymbolValue Env Sym Var)
  (if (equal? Sym (caar Env))
      (cons (list Sym Var)
            (cdr Env))
      (cons (car Env)
            (updateSymbolValue (cdr Env) Sym Var))))

(define (map Function Collection)
  (if (empty? Collection)
      '()
      (cons (Function (car )))))

(define (GreaterThan arg)
  (> (car arg) (cadr arg)))

(define (Equals arg)
  (= (car arg) (cadr arg)))

(define (LessThan arg)
  (< (car arg) (cadr arg)))

(define (And arg)
  (and (car arg) (cadr arg)))

(define (Or arg)
  (or (car arg) (cadr arg)))

(define (Not arg)
  (not (car arg)))

(define (getOpSem)
  (cond [(equal? getOpSem 'gt) GreaterThan]
        [(equal? getOpSem 'lt) LessThan]
        [(equal? getOpSem 'eq) Equals]
        [(equal? getOpSem 'and) And]
        [(equal? getOpSem 'or) Or]
        [(equal? getOpSem 'not) Not]))




(getSymbolValue '((x 1) (y 5)) 'y)
 
  