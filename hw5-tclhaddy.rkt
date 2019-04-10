#lang racket
(require racket/trace)
(provide (all-defined-out))
(require "program.rkt")

;;synchk
(define (synchk P)
  (if (list? P)
      (if (null? (cdr P))  ;; P contains just one
          (synchkStmt (car P))
          (and (synchkStmt (car P))
               (synchk (cdr P))))
      false))

(define (synchkStmt S)
  (and (list? S)
       (or
        ;; declaration
        (and (equal? (car S) 'decl)
             (equal? (length S) 2)
             (symbol? (cadr S)))
     
        ;; assignment 
        (and (equal? (car S) 'assign)
             (equal? (length S) 3)
             (symbol? (cadr S))
             (arithExpr (cadr (cdr S))))
        ;; if
        (and (equal? (car S) 'if)
             (equal? (length S) 3)
             (condExpr (cadr S))
             (synchk (cadr (cdr S)))))))

(define (arithExpr E)
  (or
   ;; number
   (number? E)
   ;; variable   
   (symbol? E)
   ;; operator operand operand   
   (and (or (equal? (car E) '+) (equal? (car E) '-) (equal? (car E) '/) (equal? (car E) '*) )
        (equal? (length E) 3)
        (arithExpr (cadr E))
        (arithExpr (cadr (cdr E))))))
  
(define (condExpr E)
  (and (list? E)
       (or
        ;; gt, lt, eq
        (boolExpr E)
        ;; or, and, 
        (and
         (or (equal? (car E) 'and) (equal? (car E) 'or))
         (equal? (length E) 3)
         (condExpr (cadr E))
         (condExpr (cadr (cdr E))))
        ;; not
        (and
         (equal? (car E) 'not)
         (equal? (length E) 2)
         (condExpr (cadr E))))))
  
(define (boolExpr E)
  (and
       (or (equal? (car E) 'gt) (equal? (car E) 'lt) (equal? (car E) 'eq))
       (arithExpr (cadr E))
       (arithExpr (cadr (cdr E)))))

;; semantics of a program
(define (sem P Env)
  (if (null? (cdr P))  ; one statement in the program
      (semstmt (car P) Env)  ;; find the semantics of the single statement
      (sem (cdr P) (semstmt (car P) Env))))  ;; else relay-chain the semantics

(define (semstmt S Env)
  (cond
    ;; declaration
    [ (equal? (car S) 'decl)   (cons (list
                                      (cadr S) 0)
                                     Env) ]
    ;; assignment: update the value of the variable
    [ (equal? (car S) 'assign) (updateValue (cadr S)
                                            (semArith (cadr (cdr S)) Env)
                                            Env) ]
    ;; if: setup a marker for the start of the if and when all is done, remove the environment till the marker (incl)
    [ (equal? (car S) 'if)  (removemarkerif (semIf (semcomplexcond (cadr S) Env) ;; condExpr
                                                 (cadr (cdr S)) ;; sequence of stmts
                                                 (cons (list '$if 0) Env))) ]
    ;; Function declaration
    [(equal? (car S) 'fundecl) (cons (list (list
                                            (caadr S) ;;Function name
                                            (cadadr S)) ;;Function args
                                           (caddr S)) ;;Function def
                                     Env)]
    
    ;; Function call
    [(equal? (car S) 'call) (removemarkerfunc (semCallFunc (caadr S) ;;Function name
                                                           (cadadr S) ;;Function params
                                                           (cons '($func 0) Env)))]))

(define (removemarkerfunc Env)
  (if (equal? (caar Env) '$func)
      (cdr Env)
      (removemarkerfunc (cdr Env))))

;; Semantics of a function call
(define (semCallFunc func-name param Env)
  (semFuncVal func-name (findValue func-name Env) param Env))

(define (semFuncVal func-name func-value param Env)
  (semFuncDef func-name (car func-value) (cadr func-value) param Env))

(define (semFuncDef func-name variable func-definition param Env)
  (sem func-definition (passArguments variable param Env)))

(define (passArguments variable param Env)
  (if (null? param)
      Env
      (passArguments (cdr variable)
                     (cdr param)
                     (cons (list (car variable)
                                 (semArith (car param) Env))
                           Env))))
;; Semantics of an if
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
  (if (and (list? (caar Env))
           (equal? v (caaar Env)))
      (list (cadaar Env)
            (cadar Env))
      (if (equal? v (caar Env))
          (cadar Env)
          (findValue v (cdr Env)))))

;; Semantics of an Arithmetic Expression
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

;; Semantics of complex conditions
(define (semcomplexcond CCond Env)
  (cond
    [ (equal? (car CCond) 'or)   (or (semcomplexcond (cadr CCond) Env)
                                     (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'and)   (and (semcomplexcond (cadr CCond) Env)
                                     (semcomplexcond (cadr (cdr CCond)) Env)) ]
    [ (equal? (car CCond) 'not)   (not (semcomplexcond (cadr CCond) Env))
                                      ]
    [ else  (semboolcond CCond Env) ]))  ;; semantics of conditions: lt, gt

;; Complete the definition
(define (semboolcond BCond Env)
  (cond
    [ (equal? (car BCond) 'gt)  (> (semArith (cadr BCond) Env)
                                   (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'lt)  (< (semArith (cadr BCond) Env)
                                   (semArith (cadr (cdr BCond)) Env)) ]
    [ (equal? (car BCond) 'eq)  (equal? (semArith (cadr BCond) Env)
                                        (semArith (cadr (cdr BCond)) Env)) ]))



