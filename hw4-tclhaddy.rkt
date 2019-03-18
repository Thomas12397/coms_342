#lang racket
(require "program.rkt")
(provide (all-defined-out))

;Author: Thomas Haddy 3/18/19

;---------------------------Problem: 1)------------------------------------------

;;Checks if a list is of size N
(define (isListSizeN Expr N)
  (and (list? Expr)
       (equal? (length Expr) N)))

;;Checks if SSeq --> Statement | Statement SSeq
(define (isSSeq Expr)
  (and (not (empty? Expr))
       (if (isListSizeN Expr 1)
           ;True
           (isStatement (car Expr))
           ;False
           (and (isStatement (car Expr))
                (isSSeq (rest Expr))))))

;;Checks if Statement --> Decl | Assign | If
(define (isStatement Expr)
  (or (isDecl Expr)
      (isAssign Expr)
      (isIf Expr)))

;;Checks if Decl --> '(decl Var)
(define (isDecl Expr)
  (and (isListSizeN Expr 2)
       (equal? (car Expr) 'decl)
       (isVar (cadr Expr))))

;;Checks if Assign --> '(assign Var ArithExpr)
(define (isAssign Expr)
  (and (isListSizeN Expr 3)
       (equal? (car Expr) 'assign)
       (isVar (cadr Expr))
       (isArithExpr (caddr Expr))))

;;Checks if If --> '(if CondExpr (SSeq))
(define (isIf Expr)
  (and (isListSizeN Expr 3)
       (equal? (car Expr) 'if)
       (isCondExpr (cadr Expr))
       (isSSeq (caddr Expr))))

;;Checks if ArithExpr --> '(Op ArithExpr ArithExpr) | Number | Var
(define (isArithExpr Expr)
  (or (and (isListSizeN Expr 3)
           (isOp (car Expr))
           (isArithExpr (cadr Expr))
           (isArithExpr (caddr Expr)))
      (number? Expr)
      (isVar Expr)))

;;Checks if Op --> + | - | * | /
(define (isOp Expr)
  (or (equal? Expr '+)
      (equal? Expr '-)
      (equal? Expr '/)
      (equal? Expr '*)))

;;Checks if CondExpr --> BCond | '(or CondExpr CondExpr) | '(and CondExpr CondExpr) |
;;                       '(not CondExpr)
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

;;Checks if BCond --> '(gt ArithExpr ArithExpr) | '(lt ArithExpr ArithExpr) |
;;                    '(eq ArithExpr ArithExpr)
(define (isBCond Expr)
  (and (isListSizeN Expr 3)
       (or (equal? (car Expr) 'gt)
           (equal? (car Expr) 'lt)
           (equal? (car Expr) 'eq))
       (isArithExpr (cadr Expr))
       (isArithExpr (caddr Expr))))

;;Checks if Var --> symbol
(define (isVar Expr)
  (symbol? Expr))

;;Takes a program (SSeq) as the input and returns true iff
;;the program belongs to L of the above grammer defined
(define synchk isSSeq)

;(synchk program1)
;(synchk program2)

;---------------------------------Problem: 2)---------------------------------

;;Filters an expression by a function
;;Func: The function
;;Expr: The expression being evaluated
(define (filterExpr Func Expr)
  (if (empty? Expr)
      '()
      (if (Func (car Expr))
          (cons (car Expr) (filterExpr Func (cdr Expr)))
          (filterExpr Func (cdr Expr)))))

;;Reduces an expression
(define (reduceExpr Func Var Expr)
  (if (empty? Expr)
      Var
      (reduceExpr Func (Func Var (car Expr)) (cdr Expr))))

;;Maps a function to the expression
;;Func: The function
;;Expr: The expression being evaluated
(define (mapToExpr Func Expr)
  (if (empty? Expr)
      '()
      (cons (Func (car Expr))
            (mapToExpr Func (cdr Expr)))))

;;Gets the variables from an enviroment
(define (getVars Env)
  (mapToExpr car Env))

;;Gets the symbol value from an environment.
;;Env: The environment
;;Sym: The symbol
(define (getSymbolValue Env Sym)
  (if (equal? (caar Env) Sym)
      (cadar Env) ;Found sym, return value
      (getSymbolValue (cdr Env) Sym))) ;Didn't find Sym, go to next Sym

;;Sets a symbol value from an environment.
;;Env: The environment
;;Sym: The symbol
;;Val: The value for that symbol
(define (setSymbolValue Env Sym Val)
  (if (equal? (caar Env) Sym)
      (cons (list Sym Val) (cdr Env)) ;Found sym, return new pair in Env
      (cons (car Env) (setSymbolValue (cdr Env) Sym Val)))) ;Didn't find sym, go to next Sym

;;Gets the 'gt arguments to be evaluated
;;args: The 'gt arguments
(define (getGreaterThanArgs Args)
  (> (car Args)
     (cadr Args)))

;;Gets the 'lt arguments to be evaluated
;;args: The 'lt arguments
(define (getLessThanArgs Args)
  (< (car Args)
     (cadr Args)))

;;Gets the 'eq to arguments to be evaluated
;;args: The 'eq to arguments
(define (getEqualArgs Args)
  (= (car Args)
     (cadr Args)))

;;Gets the 'and arguments to be evaluated
;;args: The 'and arguments
(define (getAndArgs Args)
  (and (car Args)
       (cadr Args)))

;;Gets the 'or arguments to be evaluated
;;args: The 'or arguments
(define (getOrArgs Args)
  (or (car Args)
      (cadr Args)))

;;Gets the 'not argument to be evaluated
;;args: The 'not argument
(define (getNotArg Arg)
  (not (car Arg)))

;;Gets the Op ('gt, 'lt, 'eq, etc.) and calls its respective function
;;op: The op symbol ('gt, 'lt, 'eq, etc.)
(define (getOp OpSym)
  (cond [(equal? 'gt  OpSym) getGreaterThanArgs]
        [(equal? 'lt  OpSym) getLessThanArgs]
        [(equal? 'eq  OpSym) getEqualArgs]
        [(equal? 'and OpSym) getAndArgs]
        [(equal? 'or  OpSym) getOrArgs]
        [(equal? 'not OpSym) getNotArg]))

;;Helper function for evaluating conditional expressions
;;EvalFunc: The evaluate function for conditionals
;;Env: The environment
;;OpSym: The Op symbol ('gt, 'lt, 'eq, etc.)
;;Args: The arguments of the Op
(define (evalCondExprHelper EvalFunc Env OpSym Args)
  ((getOp OpSym) (mapToExpr (lambda (Arg) (EvalFunc Env Arg)) Args)))

;;Evaluates conditional expressions
;;Env: The environment
;;Expr: The conditional expression
(define (evalCondExpr Env Expr)
  (cond [(list? Expr) (evalCondExprHelper evalCondExpr Env (car Expr) (cdr Expr))]
        [(symbol? Expr) (getSymbolValue Env Expr)]
        [else Expr]))

;;Checks if Val in in Env
;;Val: The value
;;Env: The environment
(define (isValinEnv Env Val)
  (and (not (empty? Env))
       (or (equal? Val (car Env))
           (isValinEnv (cdr Env) Val))))

;; Assigns Val to Var
;; PBlock: The parent block's symbol values
;; CBlock: The child block's symbol values
;; DeclVars: A list of declared variables
;; Var: the variable getting assigned
;; Val: the value to assign
(define (assignValToVar PBlock CBlock DeclVars Var Val)
  (if (isValinEnv DeclVars Var)
      (list PBlock (setSymbolValue CBlock Var Val) DeclVars)
      (list (setSymbolValue PBlock Var Val) CBlock DeclVars)))

;;Sets a symbol from one block to another
;;BlockToSet: The block that will be updated
;;setBlock: The block with the updated Var
;;VarToSet: The variable to be set
(define (setVars BlockToSet)
  (lambda (setBlock VarToSet)
    (setSymbolValue setBlock VarToSet (getSymbolValue BlockToSet VarToSet))))

;;Merges a variable from the child block to the parent block
;;PBlock: Parent block
;;CBlock: Child block
;;PDeclVars: Parent block declared variables
;;CDeclVars: Child block declared variables
;;Var: The variable being set
(define (mergeBlocks PBlock CBlock PDeclVars CDeclVars)
  (reduceExpr (setVars PDeclVars) PBlock
              (filterExpr (lambda (Var) (isValinEnv CBlock Var)) CDeclVars)))

;;Evaluates statements 
;;PBlock: The parent block
;;PEnv: The parent environment
;;PDeclVars: The parent declared variables
;;Expr: The expression to be evaluated
;;Sem: The sem function
(define (evalStatement PBlock PEnv PDeclVars Expr Sem)
  (case (car Expr) 
    ['decl
     (list PBlock (cons (list (cadr Expr) 0) PEnv) (cons (cadr Expr) PDeclVars))]
    ['assign
     (assignValToVar PBlock PEnv PDeclVars (cadr Expr) (caddr Expr))]
    ['if
     (if (evalCondExpr PEnv (cadr Expr))
             ((lambda (CEnv)
                (list PBlock (car CEnv) (getVars (car CEnv)))) (Sem PEnv (caddr Expr)))
             (list PBlock PEnv PDeclVars))]
    ))

;;Helper function to run a program
(define (runProgramHelper PBlock PEnv PDeclVars CBlock Sem)
  (if (empty? CBlock)
      (list PBlock PEnv PDeclVars)
      ((lambda (Var)
         (runProgramHelper (car Var) (cadr Var) (caddr Var) (cdr CBlock) Sem))
       (evalStatement PBlock PEnv PDeclVars (car CBlock) Sem))))

;;Runs the program
(define (runProgram Env Program)  
  ((lambda (NewEnv) 
     ((lambda (PBlock CBlock PDeclVars CDeclVars)
        (list PBlock (mergeBlocks PBlock CBlock PDeclVars CDeclVars)))
      (car NewEnv) (getVars (car NewEnv)) (cadr NewEnv) (caddr NewEnv)))
   (runProgramHelper Env '() '() Program runProgram)))

(define (sem Program Env)
  (runProgram Env Program))
 
  