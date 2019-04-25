#lang racket
;; Author: Thomas Haddy
(require racket/trace)
(provide (all-defined-out))
;(require "program.rkt")

#| ###################################################################### |#

;; semantics of a program = semantics of rest of the program in the context of semantics of first statement
;; Added in a Heap for references
(define (sem P Env Heap)
  (if (null? (cdr P))
      (semstmt (car P) Env Heap)
      (sem (cdr P)
           (car (semstmt (car P) Env Heap))
           (cadr (semstmt (car P) Env Heap)))))

(define (semstmt S Env Heap)
  (cond
    ;; declaration 
    [(equal? (car S) 'decl) (list (cons (list (cadr S) 0) Env) Heap)]
    ;; assignment
    [(equal? (car S) 'assign) (list (updateValue (cadr S)
                                                 (semArith (cadr (cdr S)) Env)
                                                 Env) Heap)]
    ;; if
    [(equal? (car S) 'if) (remove-marker-car (semIf (semcomplexcond (cadr S) Env) ;; condExpr
                                                    (cadr (cdr S)) ;; sequence of stmts
                                                    (cons (list '$m 0) Env) Heap))]
    ;; fundecl (fundecl (fname Paramlst) fDef) 
    [(equal? (car S) 'fundecl) (list (cons (list (cadr S)
                                                 (cadr (cdr S))) Env) Heap)]
    ;; call (call (fname Arglst))
    [(equal? (car S) 'call) (remove-marker-car (semcall (findDef (car (cadr S))
                                                                 (length (cadr (cadr S)))
                                                                 Env
                                                                 Env)
                                                        (semArithList (cadr (cadr S)) Env)
                                                        (cons (list '$m 0) Env) Heap))]
    ;;References
    [(equal? (car S) 'ref) (semref Env Heap (cadr S) (caddr S))]
    [(equal? (car S) 'deref) (semderef Env Heap (cadr S) (semArith (caddr S) Env))]
    [(equal? (car S) 'free) (semfree Env Heap (semArith (cadr S) Env))]
    [(equal? (car S) 'wref) (semWRef Env Heap (semArith (cadr S) Env) (semArith (caddr S) Env))]))

(define (semWRef env Heap dest source)
  (list env
        (cond
          [(not (has-i? Heap dest)) '(ooma)]
          [(equal? 'free (get-Heap Heap dest)) '(fma)]
          [else (update-i Heap dest source)])))

;; Does the Heap contain the index i?
;; (has-i? '((1 'fasdfsadfdsagfdsag) (2 'fdsfdsfdsfds)) 2)
;; #t
;;
;; (has-i? '((1 'fasdfsadfdsagfdsag) (2 'fdsfdsfdsfds)) 3)
;; #f
(define (has-i? Heap i)
  (if (null? Heap)
      #f
      (or (equal? i (caar Heap))
          (has-i? (cdr Heap) i))))

;; change the value of index i
;; (update-i '((1 1) (2 2)) 2 'sausage)
;; '((1 1) (2 sausage))
;;
;; (update-i '((1 1) (2 2)) 3 'sausage)
;; THIS WILL CRASH
(define (update-i Heap i v)
  (if (equal? (caar Heap) i)
      (cons (list (caar Heap) v)
            (cdr Heap))
      (cons (car Heap) (update-i (cdr Heap) i v))))

;; from the pdf
;; (semfree '((x billybobdundiddly)) '((1 23343243)) 1)
;; '(((x billybobdundiddly)) ((1 free)))
(define (semfree env Heap i)
  (if (has-i? Heap i)
      (list env (update-i Heap i 'free))
      (list env '(ooma))))

;; from the pdf
;; (semderef '((y 0)) '((1 200)) 'y 1)
;; '(((y 200)) ((1 200)))
(define (semderef env Heap dest source)
  (cond
    [(symbol? (car Heap)) (list env Heap)]
    [(equal? 'free (get-Heap Heap source)) (list env '(fma))]
    [else (list (updateValue dest (get-Heap Heap source) env) Heap)]))

;; is this entry free
;; (entry-free? '(1 free))
;; #t
;;
;; (entry-free? '(1 23324324))
;; #f
(define (entry-free? entry)  
  (equal? 'free (cadr entry)))

;; is there a free space in the Heap?
;; (Heap-free? '((1 2343) (2 free)))
;; #t
;;
;; (Heap-free? '((1 2343) (2 2343)))
;; #f
(define (Heap-free? Heap)
  (if (null? Heap)
      #f
      (or (entry-free? (car Heap))
          (Heap-free? (cdr Heap)))))

;; first free entry
;; (first-free '((1 233) (2 free) (3 free)))
;; '(2 free)
(define (first-free Heap)
  (if (entry-free? (car Heap))
      (car Heap)
      (first-free (cdr Heap))))

;; from the pdf
;; (semref '((x 0)) '((1 free)) 'x 123)
;; '(((x 1)) ((1 123)))
(define (semref env Heap var val)
  (if (Heap-free? Heap)
      (list (updateValue var (car (first-free Heap)) env)
            (update-Heap Heap val))
      (list env '(oom))))

;; (update-entry '(1 free) 23)
;; '(1 23)
(define (update-entry entry v)
  (list (car entry) v))

;; (update-Heap '((1 free) (2 free)) 123)
;; '((1 123) (2 free))
(define (update-Heap Heap v)
  (if (entry-free? (car Heap))
      (cons (update-entry (car Heap) v)
            (cdr Heap))
      (cons (car Heap)
            (update-Heap (cdr Heap) v))))

;; (get-Heap '((1 free) (2 234)) 2)
;; 234
(define (get-Heap Heap i)
  (if (equal? (caar Heap) i)
      (cadar Heap)
      (get-Heap (cdr Heap) i)))


#| This part is for functions |#

#| Two env? If the search for a function call leads to a variable, we need
   continue the search with the value of the variable, i.e., a function name,
   in the entire environment.
   Structure of environment: ((x y) ((f plist) fdef))
|#
(define (findDef fname nParams Env StaticEnv)
  (if (equal? (car (car Env)) fname) ;; calling name matches with a variable, search again
      (findDef (cadr (car Env)) nParams StaticEnv StaticEnv) ;; search from the top of environment
      ;; is this a function, does the name of the function match; does the number of params match - then
      (if (and (list? (car (car Env))) (equal? (car (car (car Env))) fname) (equal? (length (cadr (car (car Env)))) nParams))
          (list (cadr (car (car Env))) (cadr (car Env))) ;; paramlist and definitions
          ;; continue with the search search in the rest of the environment
           (findDef fname nParams (cdr Env) StaticEnv))))


;; create an addition to the enviroment using the parameters-Argvals
(define (genEnv Params Args Env)
  (if (null? Params)
      Env
      (cons (list (car Params) (car Args)) (genEnv (cdr Params) (cdr Args) Env))))

(define (semArithList Exprs Env)
  (if (null? Exprs)
      Exprs
      (cons (semArith (car Exprs) Env) (semArithList (cdr Exprs) Env))))

#|
 Rest: for creating the composition. 
 ParamsDef is a list containing (ParameterList Definition)
 Args is a list of argument values
|#
(define (semcall ParamsDef Args Env Heap)
  (semcalllow (cadr ParamsDef) (genEnv (car ParamsDef) Args Env) Heap))

#| recursively compute the function definition in a new environment |#
(define (semcalllow Def Env Heap) (sem Def Env Heap))


(define (findValue v Env) ;; update to make room for function names being assigned to variables; they do not
                          ;; have values
  (if (null? Env)         ;; couple of lines to add!!
      v
      (if (equal? v (car (car Env)))
          (cadr (car Env))
          (findValue v (cdr Env)))))

#| The addendum for the functions ends here |#

(define (semIf condVal SSeq Env Heap)
  (if condVal
      (sem SSeq Env Heap)
      Env))

(define (removemarker Env)
  (if (equal? (car (car Env)) '$m)
      (cdr Env)
      (removemarker (cdr Env))))

(define (remove-marker-car l)
  (list (removemarker (car l))
        (cdr l)))

(define (updateValue v val Env)
  (if (equal? (car (car Env)) v)
      (cons (list (car (car Env))
                  val)
            (cdr Env))
      (cons (car Env) (updateValue v val (cdr Env)))))




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

(define p0
'(
(decl x)
(decl y)
(ref x 10)
(deref y x)
)
)
(sem p0 '() '((1 free) (2 free)))
; '(((y 10) (x 1))
; ((1 10) (2 free)))
(sem p0 '() '((1 20) (2 free)))
; '(((y 10) (x 2))
; ((1 20) (2 10)))
(sem p0 '() '((1 20) (2 40)))
; '(((y 0) (x 0))
; (oom))
(define p1
'(
(decl x)
(decl y)
(ref x 10)
(wref x 30)
(deref y x)
(free x)
)
)
(sem p1 '() '((1 free) (2 free)))
; '(((y 30) (x 1)) ((1 free) (2 free)))
(define p5
'(
(decl x)
(assign x 0)
(free x)
)
)
(sem p5 '() '((1 free)))
; '(((x 0)) (ooma))
(define p6
'(
(decl x)
(deref x 1)
)
)
(sem p6 '() '((1 free)))

; '(((x 0)) (fma))
(define p2
'(
(fundecl (swap (x y)) (
(decl temp1)
(decl temp2)
(deref temp1 x)
(deref temp2 y)
(wref x temp2)
(wref y temp1)
)
)
(decl a)
(decl b)
(assign a 1)
(assign b 2)
(call (swap (a b)))
)
)
(sem p2 '() '((1 20) (2 500)))


