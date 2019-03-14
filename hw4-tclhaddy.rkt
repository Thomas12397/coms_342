#lang racket
;(require "program.rkt")
(provide (all-defined-out))

(define (symbol?) x)

(define grammar
  '
  ((Program) ((SSeq)))
  (SSeq (Statement
         (Statement SSeq)))
  (Statement (Decl
              Assign
              If))
  (Decl ("decl" Var))
  (Assign ("assign" Var ArithExpre))
  (If (("if" CondExpre (SSeq))))
  (ArithExpre (Number
              Var
              (Op ArithExpr ArithExpr)))
  (Op ("+")
      ("-")
      ("*")
      ("/"))
  (CondExpre (BCond)
             ("or" CondExpr CondExpre)
             ("and" CondExpr CondExpr)
             ("not" CondExpr CondExpr))
  (BCond ("gt" ArithExpr ArithExpr)
         ("lt" ArithExpr ArithExpr)
         ("eq" ArithExpr ArithExpr))
  (Var (symbol?))
  )

(define (synchk Program)
  )