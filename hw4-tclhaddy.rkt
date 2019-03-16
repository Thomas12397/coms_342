#lang racket
;(require "program.rkt")
(provide (all-defined-out))

;Problem: 1)

(define (synchk program)) ;TODO

(define (isSSeq Expr)
  (or (equal?) (car Expr) (Statement)
      (equal?) (car Expr) (cadr SSeq)))

(define (isStatemnt Expr)
  (and (list? Expr) (or (isDecl Expr) (isAssign Expr) (isIf Expr))))

(define (isDecl Expr)
  (and (list? Expr) (or (equal? (car Expr) 'decl) (equal? (cadr Expr) (isVar)))))

(define (isAssign Expr)
  (and (list? Expr) (and (equal? (car Expr) 'assign)
                         (equal? (cadr Expr) (isCondExpr))
                         (equal? (caadr Expr) (isArithExpr)))))

(define (isIf Expr)) ;TODO

(define (isArithExpr Expr)
  (and ())) ;TODO

(define (isOp Expr)
  (or (equal? (car Expr) '+)
      (equal? (car Expr) '-)
      (equal? (car Expr) '/)
      (equal? (car Expr) '*)))

(define (isCondExpr Expr)) ;TODO

(define (isBCond Expr)) ;TODO

(define (isVar Expr)
  (symbol?)) ;TODO

;Problem: 2)



 
  