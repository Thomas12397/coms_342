#lang racket
(require racket/trace)
(provide (all-defined-out))
;(require "program.rkt")

;; semantics of a program = semantics of rest of the program in the context of semantics of first statement
(define (sem P Env)
  (if (null? (cdr P))  ; one statement in the program 
      (semstmt (car P) Env)  ;; find the semantics of the single statement
      (sem (cdr P) (semstmt (car P) Env))))  ;; else relay-chain the semantics

(define (semstmt S Env)
  (cond
    ;; declaration 
    [ (equal? (car S) 'decl)   (cons (list (cadr S) 0) Env) ]
    ;; assignment: update the value of the variable
    [ (equal? (car S) 'assign) (updateValue (cadr S)
                                            (semArith (cadr (cdr S)) Env)
                                            Env) ]
    ;; if: setup a marker for the start of the if and when all is done, remove the environment till the marker (incl)
    [ (equal? (car S) 'if)  (removemarkerif (semIf (semcomplexcond (cadr S) Env) ;; condExpr
                                                 (cadr (cdr S)) ;; sequence of stmts
                                                 (cons (list '$if 0) Env))) ]
    [ (equal? (car S) 'fundecl) (cons (list (cadr S) 0) Env) ]
    [ (equal? (car S) 'call) (removemarkerfunc (semCallFunc (cadr S) (caddr S) Env) ;; condExpr
                                               (cadr (cdr S)) ;; sequence of stmts
                                                 (cons (list '$func 0) Env))]
    ))

(define (removemarkerfunc Env)
  (if (equal? (car (car Env)) '$func)
      (cdr Env)
      (removemarkerfunc (cdr Env))))

(define (semCallFunc funcName paramList Env)
  )

(define (semIf condVal SSeq Env)
  (if condVal
      (sem SSeq Env)
      Env))

(define (removemarkerif Env)
  (if (equal? (car (car Env)) '$if)
      (cdr Env)
      (removemarkerif (cdr Env))))

(define (updateValue v val Env)
  (if (equal? (car (car Env)) v)
      (cons (list (car (car Env))
                  val)
            (cdr Env))
      (cons (car Env) (updateValue v val (cdr Env)))))

(define (findValue v Env)
  (if (equal? v (car (car Env)))
      (cadr (car Env))
      (findValue v (cdr Env))))

(define (semArith Expr Env)
  (cond
    [ (number? Expr)          Expr ]

    [ (symbol? Expr)          (findValue Expr Env) ]
    
    [ (equal? (car Expr) '+)  (+ (semArith (cadr Expr)  Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '-)  (- (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '*)  (* (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    [ (equal? (car Expr) '/)  (/ (semArith (cadr Expr) Env)
                                 (semArith (cadr (cdr Expr)) Env)) ]
    ))

; semantics of complex conditions
(define (semcomplexcond CCond Env)
  (cond
    [ (equal? (car CCond) 'or)   (or (semcomplexcond (cadr CCond) Env)
                                     (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'and)   (and (semcomplexcond (cadr CCond) Env)
                                     (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'not)   (not (semcomplexcond (cadr CCond) Env))
                                      ]
    [ else  (semboolcond CCond Env) ]))  ;; semantics of conditions: lt, gt

; complete the definition
(define (semboolcond BCond Env)
  (cond
    [ (equal? (car BCond) 'gt)  (> (semArith (cadr BCond) Env)
                                   (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'lt)  (< (semArith (cadr BCond) Env)
                                   (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'eq)  (equal? (semArith (cadr BCond) Env)
                                        (semArith (cadr (cdr BCond)) Env)) ]))

;(define Env '((x 500) (y 100)))
;(sem Env ')



