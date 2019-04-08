#lang racket

(define p0
'(
  (fundecl (f (x)) (
                    (assign y (+ x 1)))
           )
  (decl y)
  (call (f (0)))
  )
  )

(define p1
  '(
    (fundecl (f (x)) (
                      (assign y (+ x 1)))
             )
    (decl y)
    (decl z)
    (assign z f)
    (call (z (0)))
    )
  )

(define p2
  '(
    (decl y)
    (decl z)
    (assign z f)
    (call (z (0)))
    )
  )

(define p3
  '(
    )
  )

(sem p0 '())
;; expected result for ’() input environment is
;; ’((y 1) ((f (x)) ((assign y (+ x 1)))))

(sem p1 '())
;; expected result for ’() input environment is
;; ((y 1) (z f) ((f (x)) ((assign y (+ x 1)))))

(sem p2 '(((f (x)) ((assign y (+ x 1))))))
;; expected result for ’(((f (x)) ((assign y (+ x 1)))))
;; (((f (x)) ((assign y (+ x 1)))) (y 1) (z f))

