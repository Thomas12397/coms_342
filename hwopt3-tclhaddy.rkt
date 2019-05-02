#lang racket
;; Author: Thomas Haddy 5/1/19
(provide (all-defined-out))

(define attr1 '((black (red blue white))
                (blue  (red))
                (white (blue red))))

(define attr2 '((electric (four six))
                (hybrid   (four six))
                (four     (six))))

(define attr3 '((tesla (skoda alfa bmw))
                (bmw   (skoda alfa))
                (alfa  (skoda))))

(define carlist '((red electric tesla)
                  (black hybrid bmw)
                  (blue electric bmw)
                  (red hybrid bmw)
                  (red four alfa)
                  (blue electric tesla)
                  (black four alfa)
                  (black electric skoda)))

(define (filter condition lst)
  (cond [(null? lst) '()]
        [(condition (car lst)) (cons (car lst) (filter condition (cdr lst)))]
        [(filter condition (cdr lst))]))

;;getPreferences gets the preferences of a value from choices
;; '(black (red blue white)
;; (getPreferences attr1 'black)
;; > '(red blue white)
(define (getPreferences choices value)
  (cond
    [(null? choices) '()]
    [(equal? (car (car choices)) value) (car (cdr (car choices)))]
    [else (getPreferences (cdr choices) value)]
    ))

;; isInLst? checks if the element is found in a list
;; (isInLst? carlist '(red electric tesla))
;; > #t
(define (isInLst? lst element)
  (cond [(null? lst) #f]
        [(equal? (car lst) element) #t]
        [else (isInLst? (cdr lst) element)]
        ))

;; doesExist? checks to see if a condition exists inside of a list
(define (doesExist? condition lst)
  (cond
    [(null? lst) #f]
    [(condition (car lst)) #t]
    [else (doesExist? condition (cdr lst))]
    ))

;; isEdge? checks to see if node1 points to node2 in prefs
;; (isEdge? attr1 'black 'red)
;; > #t
(define (isEdge? prefs node1 node2)
  (isInLst? (getPreferences prefs node1) node2))

;; isThereAPath? checks to see if there is a path from node1 to node2 based on choices
(define (isThereAPath? choices node1 node2)
  (cond
    [(isEdge? choices node1 node2) #t]
    [(equal? node1 node2) #f]
    [else (doesExist? (lambda (x) (isThereAPath? choices x node2)) (getPreferences choices node1))]
    ))

;; identicalOrPreferreed checks if the nodes 1 and 2 are either identical or node1 is
;; preferred over node2
(define (identicalOrPreferred choices node1 node2)
  (or (equal? node1 node2)
      (isThereAPath? choices node1 node2)))

;; prefer? checks if node1 is preferred over node2 given the attributes 1, 2, and 3
(define (prefer? attr1 attr2 attr3 node1 node2)
  (and (identicalOrPreferred attr1 (car node1) (car node2))
       (identicalOrPreferred attr2 (car (cdr node1)) (car (cdr node2)))
       (identicalOrPreferred attr3 (car (cdr (cdr node1))) (car (cdr (cdr node2))))
       (or (isThereAPath? attr1 (car node1) (car node2))
           (isThereAPath? attr2 (car (cdr node1)) (car (cdr node2)))
           (isThereAPath? attr3 (car (cdr (cdr node1))) (car (cdr (cdr node2)))))))

;;--------------------Actual algorithm for constructing the weakordering of preferences----------------------

;; level-0? checks to see if choiceX should be in level 0 of the weakorder. It compares itself to
;; the rest of choices. If there exists an edge from choiceY to choiceX, then it is false.
(define (level-0? choices attr1 attr2 attr3 choiceX)
  (not (doesExist? (lambda (choiceY) (prefer? attr1 attr2 attr3 choiceY choiceX)) choices)))

;; constructNextLevel constructs the next level for weakorder
(define (constructNextLevel choices last-level attr1 attr2 attr3)
  (cons last-level (weakorder (removeMatches choices last-level) attr1 attr2 attr3)))

;; removeMatches removes the nodes from choices once a level becomes constructed
(define (removeMatches choices currentLevel)
  (filter (lambda (currentChoice) (not (isInLst? currentLevel currentChoice))) choices))

;; weakorder lists out the choices in levels where the top level is the most preferred and the
;; bottom level is the least preferred given the attributes
(define (weakorder choices attr1 attr2 attr3)
  (if (null? choices)
      '()
      (constructNextLevel
       choices
       (filter (lambda (x) (level-0? choices attr1 attr2 attr3 x)) choices)
       attr1
       attr2
       attr3)))

;(weakorder carlist attr1 attr2 attr3)
