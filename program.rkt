#lang racket
(provide (all-defined-out))

(define program1
'(
(decl x)
(assign x 3)
(decl y)
(assign y 10)
(if (gt x 2)
(
(decl x)
(assign x y)
(assign x (+ x y))
)
)
(assign x (+ x 1))
)
)

(define program2
'(
(decl x)
(decl z)
(assign x (+ y 1))
(if (gt x 1)
((assign z 1))
)
(if (gt x 2)
((assign z 2))
)
)
  )