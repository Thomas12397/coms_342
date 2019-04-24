#lang racket ;; uncomment this line
(require "hw5-tclhaddy.rkt") ;; include your solution here
(require racket/trace)
(require "program.rkt")
(provide (all-defined-out)) 


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (myequal l1 l2)
  (if (not (equal? (length l1) (length l2)))
      false
      (myeqlow l1 l2)))

(define (myeqlow l1 l2)
  (if (null? l1)
      true
      (and (present (car l1) l2)
          (myeqlow (cdr l1) l2))))

;; does some specific course present in the course list
(define (present x lst)
  (if (null? lst)
      false
      (if (equal? x (car lst))
          true
          (present x (cdr lst)))))



(define totalpoints 0) ;; total points for this assignment
(define cnt 0)  ;; test counts

(define (utest testcnt testname testfun testpoints)
  (begin
    (write testcnt)
    (write testname)
    (write ':)
    (write testpoints)
    (writeln 'pts)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (begin
                                   (writeln exn)
                                   (writeln "Exception")
                                   (writeln "incorrect")
                                   ;(set! totalpoints (- totalpoints testpoints))
                                   ))])
      (if (eval testfun ns)
          (begin
            (writeln "correct")
            (set! totalpoints (+ totalpoints testpoints)))
          (begin
            (writeln "incorrect output")
            ;(set! totalpoints (- totalpoints testpoints))
            ))
    )
    ))

(define (hw5)
  (begin

    (writeln '************************************************)
    (writeln 'Tests-on-Q1)
    (writeln '************************************************)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q0 '(myequal (sem q0 '())
                                  '((y 10) ((f (x)) ((assign y x))))) 4)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q1 '(myequal (sem q1 '())
                                  '((z2 3) (z1 2) (y 5) ((f (x)) ((assign y x))))) 4)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q2 '(myequal (sem q2 '())
                                  '((z2 100)
                                    (z1 10) (y 110)
                                    ((f (x)) ((assign y x)))
                                    ((f (x1 x2)) ((assign y (* x1 x2)))))) 5)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q3 '(myequal (sem q3 '((z 0)))
                                  '((y 4) ((f (x)) ((assign y x))) (z 0))) 5)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q4 '(myequal (sem q4 '((z 0)))
                                  '((y 2) ((f (x)) ((assign y x))) (z 0))) 5)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q5 '(myequal (sem q5 '((z 5)))
                                  '((y 120)
                                    ((f (x))
                                     ((if (eq x 1)
                                          ((assign y x)))
                                      (if (gt x 1)
                                          ((call (f ((- x 1))))
                                           (assign y (* x y))))))
                                    (z 5)))
           9)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q6 '(myequal (sem q6 '((z 5)))
                                  '((y 120)
                                    ((g (x))
                                     ((if (eq x 1) ((assign y x))) (if (gt x 1) ((call (f ((- x 1)))) (assign y (* x y))))))
                                    ((f (x))
                                     ((if (eq x 1) ((assign y x))) (if (gt x 1) ((call (g ((- x 1)))) (assign y (* x y))))))
                                    (z 5)))
           9)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q7 '(myequal (sem q7 '())
                                  '((y 40)
                                    ((f (x)) ((fundecl (g (x)) ((assign y (* 2 x)))) (call (g (x)))))
                                    ((g (x)) ((assign y (* 3 x))))))
           9)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q8 '(myequal (sem q8 '())
                                  '((y 400)
                                    ((f (x)) ((fundecl (g (x)) ((assign y (* 2 x)))) (call (g ((+ x y) (- x y))))))
                                    ((g (z1 z2)) ((assign y (* z1 z2))))))
           9)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q9 '(myequal (sem q9 '((z 5)))
                                  '((y 120)
                                    ((f (x))
                                     ((fundecl
                                       (g (x))
                                       ((if (eq x 1) ((assign y 1)))
                                        (if (gt x 1) ((call (f ((- x 1))))
                                                      (assign y (* x y))))))
                                      (call (g (x)))))
                                    (z 5)))
           9)


    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q10 '(myequal (sem q10 '())
                                   '((z f) (y 10) ((f (x)) ((assign y x)))))
           5)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q11 '(myequal (sem q11 '())
                                   '((z f) (y 10) ((g (x1 x2))
                                                   ((call (x1 (x2)))))
                                           ((f (x)) ((assign y x)))))
           7)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q12 '(myequal (sem q12 '((w 5)))
                                   '((z f) (y 120)
                                           ((f (x1 x2))
                                            ((if (eq x1 1) ((assign y x1)))
                                             (if (gt x1 1) ((call (x2 ((- x1 1) x2)))
                                                            (assign y (* x1 y)))))) (w 5)))
           9)
    (writeln '-------------)
    (set! cnt (+ cnt 1))
    (utest cnt ':sem-q13 '(myequal (sem q13 '((w 5)))
                                   '((z f)
                                     (y 120)
                                     ((f (x1)) ((assign y x1)))
                                     ((f (x1 x2)) ((if (eq x1 1) ((assign y x1)))
                                                   (if (gt x1 1) ((call (x2 ((- x1 1) x2)))
                                                                  (assign y (* x1 y)))))) (w 5)))
           7)

    (writeln '-------------)
    (set! cnt (+ cnt 1))

    (utest cnt ':sem-q14 '(myequal (sem q14 '())
                                   '((y 20) ((f (x)) ((assign y x))) (x 0)))
           4)
    
    (writeln '---------------------------------------)
    (write "                      Total Points: ")
    (writeln totalpoints)

    
    )
)

(hw5)

