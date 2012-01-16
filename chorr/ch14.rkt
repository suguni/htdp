;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define-struct child (father mother name date eyes))

;; Oldest Generation:
(define Carl (make-child empty empty 'Carl 1926 'green))
(define Bettina (make-child empty empty 'Bettina 1926 'green))

;; Middle Generation:
(define Adam (make-child Carl Bettina 'Adam 1950 'yellow))
(define Dave (make-child Carl Bettina 'Dave 1955 'black))
(define Eva (make-child Carl Bettina 'Eva 1965 'blue))
(define Fred (make-child empty empty 'Fred 1966 'pink))

;; Youngest Generation: 
(define Gustav (make-child Fred Eva 'Gustav 1988 'brown))

;; blue-eyed-ancestor? : ftn  ->  boolean
;; to determine whether a-ftree contains a child
;; structure with 'blue in the eyes field
;; version 2: using an or-expression
(define (blue-eyed-ancestor? a-ftree)
  (cond
    [(empty? a-ftree) false]
    [else (or (symbol=? (child-eyes a-ftree) 'blue)
              (or (blue-eyed-ancestor? (child-father a-ftree))
                  (blue-eyed-ancestor? (child-mother a-ftree))))]))

;; ex 14.1.1
;; empty, Carl 둘 다 false 결과를 보인다. 
;(check-expect (blue-eyed-ancestor? empty) (blue-eyed-ancestor? Carl))

;; ex 14.1.2
(blue-eyed-ancestor? Gustav)
;; Step 실행으로 확인해본다.

;; ex 14.1.3
;; count-persons : ftn -> number
(define (count-persons a-ftree)
  (cond
    [(empty? a-ftree) 0]
    [else 
     (+ 1 (count-persons (child-father a-ftree))
        (count-persons (child-mother a-ftree)))]))

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)

;; ex 14.1.4
;; total-age : ftn number -> number
(define (total-age a-ftree year)
  (cond
    [(empty? a-ftree) 0]
    [else (+ (- year (child-date a-ftree))
             (total-age (child-father a-ftree) year)
             (total-age (child-mother a-ftree) year))]))

;; average-age : ftn number -> number
(define (average-age a-ftree year)
  (cond
    [(empty? a-ftree) 0]
    [else (/ (total-age a-ftree year)
             (count-persons a-ftree))]))

(check-expect (average-age Carl 2012) 86)
(check-expect (average-age Eva 2012) 73)
(check-expect (average-age Gustav 2012) 57.8)

;; ex 14.1.5
;; eye-colors : ftn -> list-of-colors
(define (eye-colors a-ftree)
  (cond
    [(empty? a-ftree) empty]
    [else (cons (child-eyes a-ftree)
                (append (eye-colors (child-father a-ftree))
                        (eye-colors (child-mother a-ftree))))]))

(check-expect (eye-colors Eva) (list 'blue 'green 'green))

;; ex 14.1.6
;(define (proper-blue-eyed-ancestor? a-ftree)
;  (cond
;    [(empty? a-ftree) false]
;    [else (or (proper-blue-eyed-ancestor? (child-father a-ftree))
;              (proper-blue-eyed-ancestor? (child-mother a-ftree)))]))
;항상 false 결과를 보인다.

(define (proper-blue-eyed-ancestor? a-ftree)
  (cond
    [(empty? a-ftree) false]
    [else (or (blue-eyed-ancestor? (child-father a-ftree))
              (blue-eyed-ancestor? (child-mother a-ftree)))]))

(check-expect (blue-eyed-ancestor? Eva) true)
(check-expect (proper-blue-eyed-ancestor? Eva) false)
(check-expect (proper-blue-eyed-ancestor? Gustav) true)


(define-struct node (ssn name left right))

(define bn1 (make-node 29 'a empty empty))
(define bn2 (make-node 89 'b empty empty))
(define bn3 (make-node 63 'c bn1 bn2))

;; ex 14.2.1
;; contains-bt : number btn -> boolean
(define (contains-bt n a-btree)
  (cond
    [(empty? a-btree) false]
    [else (or (= n (node-ssn a-btree))
              (contains-bt n (node-left a-btree))
              (contains-bt n (node-right a-btree)))]))

(check-expect (contains-bt 89 bn3) true)
(check-expect (contains-bt 91 bn3) false)

;; ex 14.2.2
;; search-bt : number btn -> symbol
(define (search-bt n a-btree)
  (cond 
    [(contains-bt n a-btree)
     (cond
       [(empty? a-btree) false]
       [(= n (node-ssn a-btree)) (node-name a-btree)]
       [(contains-bt n (node-left a-btree)) (search-bt n (node-left a-btree))]
       [(contains-bt n (node-right a-btree)) (search-bt n (node-right a-btree))]
       [else false])]
    [else false]))

;; ex 14.2.3
;; inorder : btn -> list-of-ssn
(define (inorder a-btree)
  (cond
    [(empty? a-btree) empty]
    [else (append (inorder (node-left a-btree))
                  (list (node-ssn a-btree))
                  (inorder (node-right a-btree)))]))

(define bn-a (make-node 10 'a empty empty))
(define bn-c (make-node 24 'c empty empty))
(define bn-f (make-node 77 'f empty empty))
(define bn-i (make-node 99 'i empty empty))
(define bn-b (make-node 15 'b bn-a bn-c))
(define bn-d (make-node 29 'd bn-b empty))
(define bn-h (make-node 95 'h empty bn-i))
(define bn-g (make-node 89 'g bn-f bn-h))
(define bn-e (make-node 63 'e bn-d bn-g))
(define tree-a bn-e)

(check-expect (inorder tree-a)
              (list 10 15 24 29 63 77 89 95 99))

;; ex 14.2.4
;; search-bst : number btn -> number(symbol)
(define (search-bst n a-btree)
  (cond
    [(empty? a-btree) false]
    [(= n (node-ssn a-btree)) (node-name a-btree)]
    [(> n (node-ssn a-btree)) (search-bst n (node-right a-btree))]
    [(< n (node-ssn a-btree)) (search-bst n (node-left a-btree))]))

(check-expect (search-bst 15 tree-a) 'b)
(check-expect (search-bst 63 tree-a) 'e)
(check-expect (search-bst 100 tree-a) false)

