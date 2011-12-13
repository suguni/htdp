;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ch 14

(define-struct child (father mother name date eyes))

; A child is a structure:
;  (make-child f m na da ec) 
; where f and m are child structures; na and ec are symbols; and da is a number. 

; A child node is (make-child f m na da ec) where
; * f and m are either
;   - empty or
;   - child nodes;
; * na and ec are symbols;
; * da is a number.

; A family-tree-node (short: ftn) is either 
; * empty; or
; * (make-child f m na da ec)
;    where f and m are ftns, na
;    and ec are symbols, and da is a number.

;; Data set
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

;; fun-for-ftn : ftn  ->  ???
(define (fun-for-ftn a-ftree)
  (cond
    [(empty? a-ftree) ...]
    [else  ; (child? a-ftree)
     (... (fun-for-ftn (child-father a-ftree)) ...
     ... (fun-for-ftn (child-mother a-ftree)) ...
     ... (child-name a-ftree) ...
     ... (child-date a-ftree) ...
     ... (child-eyes a-ftree) ...)]))

;; blue-eyed-ancestor? : ftn -> boolean
;; to determine whether a-ftree contains a child
;; structure with 'blue in the eyes field
(define (blue-eyed-ancestor? a-ftree)
  (cond
    [(empty? a-ftree) false]
    [else  ; (child? a-ftree)
     (or (blue-eyed-ancestor? (child-father a-ftree))
         (blue-eyed-ancestor? (child-mother a-ftree))
         (symbol=? (child-eyes a-ftree) 'blue))]))

;; book solution 1
;(define (blue-eyed-ancestor? a-ftree)
;  (cond
;    [(empty? a-ftree) false]
;    [else (cond
;            [(symbol=? (child-eyes a-ftree) 'blue) true]
;            [(blue-eyed-ancestor? (child-father a-ftree)) true]
;            [(blue-eyed-ancestor? (child-mother a-ftree)) true]
;            [else false])]))

;; book solution 2
;(define (blue-eyed-ancestor? a-ftree)
;  (cond
;    [(empty? a-ftree) false]
;    [else (or (symbol=? (child-eyes a-ftree) 'blue)
;              (or (blue-eyed-ancestor? (child-father a-ftree))
;                  (blue-eyed-ancestor? (child-mother a-ftree))))]))

;; test
(check-expect (blue-eyed-ancestor? Carl) false)
(check-expect (blue-eyed-ancestor? Gustav) true)

;; ex 14.1.1
;; hand check a book solution 2
;  (blue-eyed-ancestor? Carl)
;= (blue-eyed-ancestor? (make-child empty empty 'Carl 1926 'green))
;= (cond
;    [(empty? (make-child empty empty 'Carl 1926 'green)) false]
;    [else (or (symbol=? (child-eyes (make-child empty empty 'Carl 1926 'green)) 'blue)
;              (or (blue-eyed-ancestor? (child-father (make-child empty empty 'Carl 1926 'green)))
;                  (blue-eyed-ancestor? (child-mother (make-child empty empty 'Carl 1926 'green)))))]))
;= (or (symbol=? (child-eyes (make-child empty empty 'Carl 1926 'green)) 'blue)
;      (or (blue-eyed-ancestor? (child-father (make-child empty empty 'Carl 1926 'green)))
;          (blue-eyed-ancestor? (child-mother (make-child empty empty 'Carl 1926 'green)))))]))
;= (or (symbol=? 'green 'blue)
;      (or (blue-eyed-ancestor? empty)
;          (blue-eyed-ancestor? empty)))
;= (or false
;      (or false false))
;= false

;  (blue-eyed-ancestor? empty)
;= (cond
;    [(empty? empty) false]
;    [else (or (symbol=? (child-eyes empty) 'blue)
;              (or (blue-eyed-ancestor? (child-father empty))
;                  (blue-eyed-ancestor? (child-mother empty))))])
;= (cond
;    [true false]
;    [else (or (symbol=? (child-eyes empty) 'blue)
;              (or (blue-eyed-ancestor? (child-father empty))
;                  (blue-eyed-ancestor? (child-mother empty))))])
;= false

;; 14.1.2
;  (blue-eyed-ancestor? empty)
;= (cond
;    [(empty? empty) false]
;    [else (cond
;            [(symbol=? (child-eyes empty) 'blue) true]
;            [(blue-eyed-ancestor? (child-father empty)) true]
;            [(blue-eyed-ancestor? (child-mother empty)) true]
;            [else false])])
;= (cond
;    [true false]
;    [else (cond
;            [(symbol=? (child-eyes empty) 'blue) true]
;            [(blue-eyed-ancestor? (child-father empty)) true]
;            [(blue-eyed-ancestor? (child-mother empty)) true]
;            [else false])])
;= false

;  (blue-eyed-ancestor? Gustav)
;= (blue-eyed-ancestor? (make-child Fred Eva 'Gustav 1988 'brown))
;= (cond
;    [(empty? (make-child Fred Eva 'Gustav 1988 'brown)) false]
;    [else
;     (cond
;       [(symbol=? 
;         (child-eyes (make-child Fred Eva 'Gustav 1988 'brown))
;         'blue)
;        true]
;       [(blue-eyed-ancestor?
;         (child-father (make-child Fred Eva 'Gustav 1988 'brown)))
;        true]
;       [(blue-eyed-ancestor?
;         (child-mother (make-child Fred Eva 'Gustav 1988 'brown)))
;        true]
;       [else false])])
;
;= (cond
;    [(symbol=? 'brown 'blue) true]
;    [(blue-eyed-ancestor? Fred) true]
;    [(blue-eyed-ancestor? Eva) true]
;    [else false])
;
;= (cond
;    [(blue-eyed-ancestor? (make-child empty empty 'Fred 1966 'pink)) true]
;    [(blue-eyed-ancestor? (make-child Carl Bettina 'Eva 1965 'blue)) true]
;    [else false])
;
;= (cond
;    [(cond
;       [(symbol=? 'pink 'blue) true]
;       [(blue-eyed-ancestor? empty) true]
;       [(blue-eyed-ancestor? empty) true]
;       [else false]) true]
;    [(cond
;       [(symbol=? 'blue 'blue) true]
;       [(blue-eyed-ancestor? Carl) true]
;       [(blue-eyed-ancestor? Bettina) true]
;       [else false]) true]
;    [else false])
;
;= (cond
;    [(cond
;       [false true]
;       [false true]
;       [false true]
;       [else false]) true]
;    [(cond
;       [true true]
;       [false true]
;       [(cond
;          [(symbol=? 'green 'blue) true]
;          [(blue-eyed-ancestor? empty) true]
;          [(blue-eyed-ancestor? empty) true]
;          [else false]) true]
;       [else false]) true]
;    [else false])
;
;= (cond
;    [false true]
;    [(cond
;       [true true]
;       [false true]
;       [(cond
;          [false true]
;          [false true]
;          [false true]
;          [else false]) true]
;       [else false]) true]
;    [else false])
;
;= (cond
;    [true true]
;    [else false])
;
;= true

;; ex 14.1.4
;; count-persons : ftn -> number
(define (count-persons a-ftree)
  (cond
    [(empty? a-ftree) 0]
    [else
      (+ 1
         (count-persons (child-father a-ftree))
         (count-persons (child-mother a-ftree)))]))

;; test
(check-expect (count-persons Fred) 1)
(check-expect (count-persons Dave) 3)
(check-expect (count-persons Gustav) 5)

;; ex 14.1.4
;; average-age : ftn -> number
(define (average-age a-ftree current-year)
  (cond
    [(empty? a-ftree) 0]
    [else (/ (sum-age a-ftree current-year) (count-persons a-ftree))]))

; Ages
; Gustav : 23
; Eva : 46
; Fred : 45
; Adam : 61
; Dave : 56
; Carl : 85
; Bettina : 85

;; Test
(check-expect (average-age Fred 2011) 45/1)
(check-expect (average-age Dave 2011) 226/3)
(check-expect (average-age Gustav 2011) 284/5)

;; sum-age : ftn number -> number
(define (sum-age a-ftree current-year)
  (cond
    [(empty? a-ftree) 0]
    [else
     (+ (- current-year (child-date a-ftree))
        (sum-age (child-father a-ftree) current-year)
        (sum-age (child-mother a-ftree) current-year))]))

;; Tests
(check-expect (sum-age Fred 2011) 45)
(check-expect (sum-age Dave 2011) (+ 56 85 85))
(check-expect (sum-age Gustav 2011) (+ 23 46 45 85 85))

;; ex 14.1.5
;; eye-colors : ftn -> list-of-symbols
(define (eye-colors a-ftree)
  (cond
    [(empty? a-ftree) empty]
    [else
     (cons (child-eyes a-ftree)
           (append (eye-colors (child-father a-ftree))
                   (eye-colors (child-mother a-ftree))))]))

;; test
(check-expect (eye-colors Carl)
              (list 'green))
(check-expect (eye-colors Adam)
              (list 'yellow 'green 'green))
(check-expect (eye-colors Gustav)
              (list 'brown 'pink 'blue 'green 'green)) ;; order check

;; ex 14.1.6
;; proper-blue-eyed-ancestor? ftn -> boolean
;; to determine whether a-ftree has a blue-eyed ancestor
;(define (proper-blue-eyed-ancestor? a-ftree)
;  (cond
;    [(empty? a-ftree) false]
;    [else
;     (or
;      (proper-blue-eyed-ancestor? (child-father a-ftree))
;      (proper-blue-eyed-ancestor? (child-mother a-ftree)))]))
;; (proper-blue-eyed-ancestor? A)
;; 어떤 a-ftree가 인자로 넘어가든 false가 된다.
;; 자신이 아닌 부모만을 검사하도록 코드가 짜여져있고,
;; 결국에는 모든 경우 empty가 되므로 false가 된다.

;; revised proper-blue-eyed-ancestor?
(define (proper-blue-eyed-ancestor? a-ftree)
  (cond
    [(empty? a-ftree) false]
    [else (or
           (blue-eyed-ancestor? (child-father a-ftree))
           (blue-eyed-ancestor? (child-mother a-ftree)))]))

;; tests
(check-expect (proper-blue-eyed-ancestor? empty) false)
(check-expect (proper-blue-eyed-ancestor? Carl) false)
(check-expect (proper-blue-eyed-ancestor? Eva) false)
(check-expect (proper-blue-eyed-ancestor? Gustav) true)

;; 14.2
(define-struct node (ssn name left right))

;; Binray Tree is
;;  false or
;;  (make-node soc pn lft rgt)
;;   where soc - number, pn - symbol, lft and rgt - Binray Tree

;; ex 14.2.1
;; tree image : ch14-ex14.2.1.png
;; contains-bt : number binary-tree -> boolean
(define (contains-bt n b-t)
  (cond
    [(false? b-t) false]
    [else
     (or
       (= n (node-ssn b-t))
       (contains-bt n (node-left b-t))
       (contains-bt n (node-right b-t)))]))

;; test
(define t1 (make-node 15 'd false (make-node 24 'i false false)))
(define t2 (make-node 15 'd (make-node 87 'h false false) false))
(define t3 false)

(check-expect (contains-bt 15 t1) true)
(check-expect (contains-bt 10 t1) false)
(check-expect (contains-bt 87 t2) true)
(check-expect (contains-bt 76 t2) false)
(check-expect (contains-bt 15 t3) false)

;; ex 14.2.2
;; search-bt : soc binary-tree -> pn
(define (search-bt n bt)
  (cond
    [(false? bt) false]
    [(= n (node-ssn bt)) (node-name bt)]
    [(contains-bt n (node-left bt)) (search-bt n (node-left bt))]
    [(contains-bt n (node-right bt)) (search-bt n (node-right bt))]
    [else false]))
;; left, right에서 찾을때는 비효율적.
;; left, right를 대상으로 search-bt 한 후 결과에 따라(false or node)
;; 반환하도록 하면 검색양이 줄어들 텐데, 아직 let이 없어서...

;; test
(check-expect (search-bt 15 t1) 'd)
(check-expect (search-bt 10 t1) false)
(check-expect (search-bt 87 t2) 'h)
(check-expect (search-bt 76 t2) false)
(check-expect (search-bt 15 t3) false)

;; ex 14.2.3
;; inorder : binary-search-tree -> list-of-ssn
(define (inorder bst)
  (cond
    [(false? bst) empty]
    [else
     (append
      (inorder (node-left bst))
      (list (node-ssn bst))
      (inorder (node-right bst)))]))

;; test
(define tree-A (make-node 63 'a
                          (make-node 29 'b
                                     (make-node 15 'c
                                                (make-node 10 'd false false)
                                                (make-node 24 'e false false))
                                     false)
                          (make-node 89 'f
                                     (make-node 77 'g false false)
                                     (make-node 95 'h
                                                false
                                                (make-node 99 'i false false)))))
(check-expect (inorder tree-A) (list 10 15 24 29 63 77 89 95 99))

;; ex 14.2.4
;; search-bst : number(ssn) binary-search-tree -> symbol(name) or false
(define (search-bst n bst)
  (cond
    [(false? bst) false]
    [(= n (node-ssn bst)) (node-name bst)]
    [(> n (node-ssn bst)) (search-bst n (node-right bst))]
    [else (search-bst n (node-left bst))])) ;; (< n (node-ssn bst))

;; tests
(check-expect (search-bst 77 tree-A) 'g)
(check-expect (search-bst 40 tree-A) false)

;; ex 14.2.5
;; create-bst : binary-search-tree number(ssn) symbol(name) -> binary-search-tree
(define (create-bst bst soc pn)
  (cond
    [(false? bst) (make-node soc pn false false)]
    [(> soc (node-ssn bst)) (make-node (node-ssn bst) 
                                       (node-name bst) 
                                       (node-left bst)
                                       (create-bst (node-right bst) soc pn))]
    [(< soc (node-ssn bst)) (make-node (node-ssn bst) 
                                       (node-name bst) 
                                       (create-bst (node-left bst) soc pn)
                                       (node-right bst))]
    [else (error "Invalid SSN Number")]))

;; tests
(check-expect (create-bst false 66 'a) (make-node 66 'a false false))
(check-expect (create-bst (create-bst false 66 'a) 53 'b)
              (make-node 66 'a
                         (make-node 53 'b false false)
                         false))
(check-expect (create-bst
               (create-bst
                (create-bst
                 (create-bst
                  (create-bst
                   (create-bst 
                    (create-bst 
                     (create-bst
                      (create-bst false 63 'a)
                      29 'b)
                     15 'c) 
                    10 'd) 
                   24 'e)
                  89 'f)
                 77 'g)
                95 'h)
               99 'i) tree-A)

;; ex 14.2.6
;; create-bst-from-list : list-of-number/name -> binary-search-tree
(define (create-bst-from-list lonns)
  (cond
    [(empty? lonns) false]
    [else
     (create-bst (create-bst-from-list (rest lonns))
                 (first (first lonns))
                 (second (first lonns)))]))

;; tests
(define sample ;; 위에서 정의한 tree-A에 맞게 name을 변경했다. 순서는 동일.
  '((99 i)
    (77 g)
    (24 e)
    (10 d)
    (95 h)
    (15 c)
    (89 f)
    (29 b)
    (63 a)))
(check-expect (create-bst-from-list sample) tree-A)

;; size : WP -> number
(define (size a-wp)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp)) (+ 1 (size (rest a-wp)))]
    [else (+ (size (first a-wp)) (size (rest a-wp)))]))
    
;; ex 14.3.1
;; - a-wp가 empty면 0
;; - a-wp의 first가 symbol이면 a-wp의 rest로 size 계산한 결과에 +1
;; - a-wp의 first가 symbol이 아니면(list)이면,
;;   a-wp의 first에 대한 size와 rest에 size의 합

;; tests
(check-expect (size empty) 0)
(check-expect (size (cons 'One empty)) 1)
(check-expect (size (cons (cons 'One empty) empty)) 1)

;; ex 14.3.2
;; occurs1 : wp symbol -> number
(define (occurs1 a-wp s)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp))
     (cond
       [(symbol=? (first a-wp) s)
        (+ 1 (occurs1 (rest a-wp) s))]
       [else
        (occurs1 (rest a-wp) s)])]
    [else (occurs1 (rest a-wp) s)]))

;; tests
(check-expect (occurs1 empty 'One) 0)
(check-expect (occurs1 (cons 'One empty) 'One) 1)
(check-expect (occurs1 (cons (cons 'One empty) empty)  'One) 0)
(check-expect (occurs1 '(One Two) 'One) 1)
(check-expect (occurs1 '(One (Two One) (Two Three (Four One) Five)) 'One) 1)

;; occurs2 : wp symbol -> number
(define (occurs2 a-wp s)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp))
     (cond
       [(symbol=? (first a-wp) s)
        (+ 1 (occurs2 (rest a-wp) s))]
       [else
        (occurs2 (rest a-wp) s)])]
    [else
     (+ (occurs2 (first a-wp) s)
        (occurs2 (rest a-wp) s))]))

;; tests
(check-expect (occurs2 empty 'One) 0)
(check-expect (occurs2 (cons 'One empty) 'One) 1)
(check-expect (occurs2 (cons (cons 'One empty) empty)  'One) 1)
(check-expect (occurs2 '(One Two) 'One) 1)
(check-expect (occurs2 '(One (Two One) (Two Three (Four One) Five)) 'One) 3)
