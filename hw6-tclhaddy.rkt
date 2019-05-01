#lang racket
;; Author: Thomas Haddy 4/25/19
(provide (all-defined-out))
(require "program.rkt")

#| ###################################################################### |#

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

;; semantics of a program = semantics of rest of the program in the context of semantics of first statement
;; Added in a Heap for references
(define (sem P Env Heap)
  (if (null? (cdr P))
      (semstmt (car P) Env Heap)
      (sem (cdr P)
           (car (semstmt (car P) Env Heap))
           (cadr (semstmt (car P) Env Heap)))))

;; Semantics of a Statement
(define (semstmt S Env Heap)
  (cond
    ;; declaration 
    [(equal? (car S) 'decl) (list (cons (list (cadr S) 0) Env) Heap)]
    ;; assignment
    [(equal? (car S) 'assign) (list (updateValue (cadr S)
                                                 (semArith (cadr (cdr S)) Env)
                                                 Env) Heap)]
    ;; if
    [(equal? (car S) 'if) (removemarkernew (semIf (semcomplexcond (cadr S) Env) ;; condExpr
                                                    (cadr (cdr S)) ;; sequence of stmts
                                                    (cons (list '$m 0) Env) Heap))]
    ;; fundecl (fundecl (fname Paramlst) fDef) 
    [(equal? (car S) 'fundecl) (list (cons (list (cadr S)
                                                 (cadr (cdr S))) Env) Heap)]
    ;; call (call (fname Arglst))
    [(equal? (car S) 'call) (removemarkernew (semcall (findDef (car (cadr S))
                                                                 (length (cadr (cadr S)))
                                                                 Env
                                                                 Env)
                                                        (semArithList (cadr (cadr S)) Env)
                                                        (cons (list '$m 0) Env) Heap))]
    ;;References
    [(equal? (car S) 'ref) (semRef Env Heap (car (cdr S)) (car (cdr (cdr S))))]
    [(equal? (car S) 'deref) (semDeref Env Heap (car (cdr S)) (semArith (car (cdr (cdr S))) Env))]
    [(equal? (car S) 'free) (semFree Env Heap (semArith (car (cdr S)) Env))]
    [(equal? (car S) 'wref) (semWRef Env Heap (semArith (car (cdr S)) Env) (semArith (car (cdr (cdr S))) Env))]
    ))

;; Semantics of ref
(define (semRef Env Heap Var Value)
  (if (isHeapFree? Heap)
      ;;True: Update the env with reference since there is 'free' in heap
      (list (updateValue Var (car (getFirstFreeEntry Heap)) Env)
            (updateHeap Heap Value))
      ;;False: Error, no free entry --> put 'oom' in Env
      (list Env '(oom))))

;; Semantics of deref
(define (semDeref Env Heap Dst Src)
  (cond
    [(symbol? (car Heap)) (list Env Heap)]
    ;;Dst index had value 'free' --> print 'fma'
    [(equal? 'free (getHeapValue Heap Src)) (list Env '(fma))]
    ;;Update the heap at index Dst with value Src
    [else (list (updateValue Dst (getHeapValue Heap Src) Env) Heap)]))

;; Semantics of free
(define (semFree Env Heap Index)
  (if (hasIndex? Heap Index)
      ;;True: Free the reference
      (list Env (updateIndex Heap Index 'free))
      ;;False: Index wasn't found in heap --> Print 'ooma'
      (list Env '(ooma))))

;; Semantics of a wref
(define (semWRef Env Heap Dst Src)
  (list Env
        (cond
          ;;Dst index had value 'free' --> print 'fma'
          [(equal? 'free (getHeapValue Heap Dst)) '(fma)]
          ;;Dst index wasn't found in heap --> print 'ooma' 
          [(not (hasIndex? Heap Dst)) '(ooma)]
          ;;Update the heap at index Dst with value Src
          [else (updateIndex Heap Dst Src)])))

;; hasIndex? goes through the heap looking for the Index given
;; Return: returns true if found, false if not found
(define (hasIndex? Heap Index)
  (if (null? Heap)
      ;;True: Index wasn't found in heap. Return false
      #f
      ;;False: Index will either be found here, or recurse the heap until it does
      (or (equal? Index (car (car Heap)))
          (hasIndex? (cdr Heap) Index))))

;; updateIndex updates index Index with value Value, assuming it's there.
;; Return: If true, returns the newly constructed heap. Otherwise the index to be updated
;;         was never found, so it returns false
(define (updateIndex Heap Index Value)
  (if (null? Heap)
      ;;True
      #f
      ;;False
      (if (equal? (car (car Heap)) Index)
          ;;True: Append the new index value to the heap
          (cons (list (car (car Heap)) Value) (cdr Heap))
          ;;False: Recurse through the heap until the Index is found
          (cons (car Heap) (updateIndex (cdr Heap) Index Value)))))

;; isHeapFree? recurses through the heap looking for 'free'
;; Return: True if 'free' was found in the heap, false otherwise
(define (isHeapFree? Heap)
  (if (null? Heap)
      ;;True
      #f
      ;;False: Check if front of heap has free entry, otherwise recurse the rest of heap
      (or (isEntryFree? (car Heap))
          (isHeapFree? (cdr Heap)))))

;; isEntryFree? checks if a given entry in the heap contains 'free' in it.
;; Return: True if 'free' is in entry, otherwise false
(define (isEntryFree? Entry)  
  (equal? 'free (car (cdr Entry))))

;; getFirstFreeEntry will get the first free entry in the heap
;; Return: returns the first entry starting from the left of the heap that has 'free' in it,
;;         false otherwise 
(define (getFirstFreeEntry Heap)
  (if (null? Heap)
      ;;True
      #f
      ;;False
      (if (isEntryFree? (car Heap))
          ;;True: Return the entry with 'free' in it
          (car Heap)
          ;;False: Recurse until we find 'free' entry in heap
          (getFirstFreeEntry (cdr Heap)))))

;; updateEntry updates a given entry Entry with value Value
;; Return: The newly updated entry
(define (updateEntry Entry Value)
  (list (car Entry) Value))

;; updateHeap will update the heap with the given value Value. It will put the Value in
;; the first free spot in the heap, starting from the left.
;; Return: returns the updated heap with Value
(define (updateHeap Heap Value)
  (if (isEntryFree? (car Heap))
      ;;True: Free spot was found, update the heap
      (cons (updateEntry (car Heap) Value)
            (cdr Heap))
      ;;False: Recurse through heap until 'free' is found
      (cons (car Heap)
            (updateHeap (cdr Heap) Value))))

;; getHeapValue gets the heap value at a given index
;; Return: Heap value at given index
(define (getHeapValue Heap Index)
  (if (equal? (car (car Heap)) Index)
      ;;True: return the heap value
      (car (cdr (car Heap)))
      ;;False: Recurse through heap looking for index
      (getHeapValue (cdr Heap) Index)))


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

(define (removemarkernew l)
  (list (removemarker (car l))
        (cdr l)))

(define (updateValue v val Env)
  (if (equal? (car (car Env)) v)
      (cons (list (car (car Env))
                  val)
            (cdr Env))
      (cons (car Env) (updateValue v val (cdr Env)))))



; semantics of Arithmetic expressions
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