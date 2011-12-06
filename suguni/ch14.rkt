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

