#lang racket
;;Author: Thomas Haddy 4/30/19

(provide (all-defined-out))



(define attr1
'(
  (black (red blue white)) ;; black is preferred to red, blue and white
  (blue (red)) ;; blue is preferred to red
  (white (blue red)) ;; white is preferred to blue and red
  ))

(define attr2
'(
  (electric (four six))
  (hybrid (four six))
  (four (six))))

(define attr3
'(
  (tesla (skoda alfa bmw))
  (bmw (skoda alfa))
  (alfa (skoda))))

(define carlist
'(
  (red electric tesla)
  (black hybrid bmw)
  (blue electric bmw)
  (red hybrid bmw)
  (red four alfa)
  (blue electric tesla)
  (black four alfa)
  (black electric skoda)))

(weakorder carlist attr1 attr2 attr3)
'(
  ( (black hybrid bmw)
    (blue electric tesla)
    (black electric skoda)
    )
  ( (red electric tesla)
    (blue electric bmw)
    (red hybrid bmw)
    (black four alfa)
    )
  ( (red four alfa)
    )
  )


