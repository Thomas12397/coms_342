#lang racket
(provide (all-defined-out))

(define q0
  '(
    (fundecl (f (x)) (
                       (assign y x)
                     )
    )
    (decl y)
    (call (f (10)))
    )
)

(define q1
  '(
    (fundecl (f (x)) (
                       (assign y x)
                     )
    )
    (decl y)
    (decl z1)
    (decl z2)
    (assign z1 2)
    (assign z2 3)
    (call (f ((+ z1 z2))))
    )
)

(define q2
  '(
    (fundecl (f (x1 x2)) (
                          (assign y (* x1 x2))
                        )
    )            
    (fundecl (f (x)) (
                       (assign y x)
                     )
    )
    (decl y)
    (decl z1)
    (decl z2)
    (assign z1 10)
    (assign z2 100)
    (call (f ((+ z1 z2))))
    )
)

(define q3
  '(
     (fundecl (f (x)) (
                        (assign y x)
                      )
     )         
     (decl y)
     (if (lt z 10) (
                     (fundecl (f (x)) (
                                        (assign y (* 2 x))
                                      )
                     )
                     (call (f (2)))
                   )
     )    
   )
)  

(define q4
  '(
     (fundecl (f (x)) (
                        (assign y x)
                      )
     )         
     (decl y)
     (if (lt z 10) (
                     (fundecl (f (x)) (
                                        (assign y (* 2 x))
                                      )
                     ) 
                   )   
     )
     (call (f (2)))
   )
)

(define q5
  '(
     (fundecl (f (x))(
                        (if (eq x 1) (
                                      (assign y x)
                                     )
                        )
                        (if (gt x 1) (
                                      (call (f ((- x 1))))
                                      (assign y (* x y))
                                     )
                        )    
                      )
     )
     (decl y)
     (call (f (z)))         
   ) 
)

(define q6
  '(
     (fundecl (f (x)) (
                        (if (eq x 1) ( (assign y x) ))
                        (if (gt x 1) ( (call (g ((- x 1))))
                                       (assign y (* x y))))
                      )        
     )
     (fundecl (g (x)) (
                        (if (eq x 1) ( (assign y x) ))
                        (if (gt x 1) ( (call (f ((- x 1))))
                                       (assign y (* x y)))) 
                      )
     )
     (decl y)
     (call (f (z)))
   )
)  

(define q7
  '(
    (fundecl (g (x)) ( (assign y (* 3 x))))
    (fundecl (f (x)) (
                       (fundecl (g (x)) ( (assign y (* 2 x))))
                       (call (g (x)))
                     )
    )
    (decl y)
    (call (f (20)))
   )
)

(define q8
  '(
    (fundecl (g (z1 z2)) ( (assign y (* z1 z2))))
    (fundecl (f (x)) (
                       (fundecl (g (x)) ( (assign y (* 2 x))))
                       (call (g ((+ x y) (- x y))))
                     )
    )
    (decl y)
    (call (f (20)))
   )
)  

(define q9
  '(
    (fundecl (f (x)) (
                      (fundecl (g (x)) ( (if (eq x 1) ((assign y 1)))
                                         (if (gt x 1) ((call (f ((- x 1))))
                                                       (assign y (* x y))))
                                       ))
                      (call (g (x)))
                               
                     ))
    (decl y)
    (call (f (z)))
   )
)  


(define q10
  '(
     (fundecl (f (x)) ( (assign y x) ))
     (decl y)
     (decl z)
     (assign z f)
     (call (z (10)))
   )
)

(define q11
  '(
     (fundecl (f (x)) ((assign y x)))
     (fundecl (g (x1 x2)) ((call (x1 (x2)))))
     (decl y)
     (decl z)
     (assign z f)
     (call (g (z 10)))
   )
)

(define q12
  '(
    (fundecl (f (x1 x2)) ( (if (eq x1 1) ((assign y x1)))
                           (if (gt x1 1) ((call (x2 ((- x1 1) x2)))
                                          (assign y (* x1 y))))))
    (decl y)
    (decl z)
    (assign z f)
    (call (f (w z)))
   )
)

(define q13
  '(
    (fundecl (f (x1 x2)) ( (if (eq x1 1) ((assign y x1)))
                           (if (gt x1 1) ((call (x2 ((- x1 1) x2)))
                                          (assign y (* x1 y))))))
    (fundecl (f (x1)) ( (assign y x1) ))
    (decl y)
    (decl z)
    (assign z f)
    (call (f (w z)))
   )
)

(define q14
  '(
    (decl x)
    (fundecl (f (x)) ((assign y x)))
    (decl y)
    (call (f (20)))
   )
)  
  
         

