;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ch 18.

;; ex 18.1.1
;; book

;; ex 18.1.2

;; 1.
;(local ((define x 10)
;        (y (+ x x)))
;  y)
;; [ERROR] local: expected a definition, but found a part

;; 2.
;(local ((define (f x) (+ (* x x) (* 3 x) 15))
;        (define x 100)
;        (define f@100 (f x)))
;  f@100 x)
;; [ERROR] local: expected only one expression after the local definitions, but found 1 extra part

;; 3.
;(local ((define (f x) (+ (* x x) (* 3 x) 14))
;        (define x 100)
;        (define f (f x)))
;  f)
;; [ERROR] local: f was defined locally more than once

;; ex 18.1.3
;; 1. X - a-nat-num
;; 2. O
;; 3. X - (define f x ...)

(define (D x y)
  (local ((define x2 (* x x))
          (define y2 (* y y)))
    (sqrt (+ x2 y2))))
;; (+ (D 0 1) (D 3 4))

;; ex 18.1.4
;; 불가능하다. 실행 중간에 확인할 수 있는 방법이 없고 실행이 완료되면 해당 정의가 사라진다.

;; ex 18.1.5
;1.
;(local ((define (x y) (* 3 y)))
;  (* (x 2) 5))
;=>
;(define (x' y) (* 3 y))
;(* (x' 2) 5)
;=>
;(define (x' y) (* 3 y))
;(* (* 3 2) 5)

;2.
;(local ((define (f c) (+ (* (/ 9 5) c) 32)))
;  (- (f 0) (f 10)))
;=>
;(define (f' c) (+ (* (/ 9 5) c) 32))
;(- (f' 0) (f' 10))
;=> ...
;(- (+ (* (/ 9 5) 0) 32) (f' 10))
;(- 32 (f' 10))
;(- 32 (+ (* (/ 9 5) 10) 32))
;(- 32 50)
;-18

;3.
;(local ((define (odd? n)
;          (cond
;            [(zero? n) false]
;            [else (even? (sub1 n))]))
;        (define (even? n)
;          (cond
;            [(zero? n) true]
;            [else (odd? (sub1 n))])))
;  (even? 1))
;=>
;(define (odd?' n)
;  (cond
;    [(zero? n) false]
;    [else (even?' (sub1 n))]))
;(define (even?' n)
;  (cond
;    [(zero? n) true]
;    [else (odd?' (sub1 n))]))
;(even?' 1)
;=>
;(cond
;  [(zero? 1) true]
;  [else (odd?' (sub1 1))])
;=>
;(odd?' (sub1 1))
;=>
;(cond
;  [(zero? 0) false]
;  [else (even?' (sub1 0))])
;=>
;false

;4.
;(+ (local ((define (f x) (g (+ x 1) 22))
;           (define (g x y) (+ x y)))
;     (f 10))
;   555)
;=>
;(define (f' x) (g' (+ x 1) 22))
;(define (g' x y) (+ x y))
;(+ (f' 10) 555)
;=> ...
;(+ (g' (+ 10 1) 22) 555)
;(+ (g' 11 22) 555)
;(+ (+ 11 22) 555)
;(+ 33 555)
;588

;5.
(define (h n)
  (cond
    [(= n 0) empty]
    [else (local ((define r (* n n)))
            (cons r (h (- n 1))))]))
;(h 2)

;(cond
;  [(= 2 0) empty]
;  [else (local ((define r (* 2 2)))
;          (cons r (h (- 2 1))))])
;
;(cond
;  [else (local ((define r (* 2 2)))
;          (cons r (h (- 2 1))))])
;
;(local ((define r (* 2 2)))
;  (cons r (h (- 2 1))))
;
;(define r_0 4)
;(cons r_0 (h 1))
;
;(cons r_0
;      (cond
;        [(= 1 0) empty]
;        [else (local ((define r (* 1 1)))
;                (cons r (h (- 1 1))))]))
;
;(cons r_0
;      (local ((define r (* 1 1)))
;        (cons r (h (- 1 1)))))
;
;(define r_1 1)
;(cons r_0
;      (cons r_1 (h 0)))
;
;(cons r_0
;      (cons r_1
;              (cond
;                [(= 0 0) empty]
;                [else (local ((define r (* 0 0 )))
;                        (cons r (h (- 0 1))))])))
;
;(cons r_0 (cons r_1 empty))
;
;(cons 4 (cons 1 empty))
;; stepper에서는 r_0, r_1이 먼저 치환됨.

;; sort함수가 원래 있다고 해서 변경. 아래 풀이는 sort로 함
(define (s-sort alon)
  (local
    ((define (sort alon)
       (cond
         [(empty? alon) empty]
         [(cons? alon) (insert (first alon)
                               (sort (rest alon)))]))
     (define (insert an alon)
       (cond
         [(empty? alon) (cons an empty)]
         [else (cond
                 [(> an (first alon)) (cons an alon)]
                 [else (cons (first alon)
                             (insert an (rest alon)))])])))
    (sort alon)))

;(sort (list 2 1 3))
;=>
;(local
;  ((define (sort alon)
;     (cond
;       [(empty? alon) empty]
;       [(cons? alon) (insert (first alon)
;                             (sort (rest alon)))]))
;   (define (insert an alon)
;     (cond
;       [(empty? alon) (cons an empty)]
;       [else (cond
;               [(> an (first alon)) (cons an alon)]
;               [else (cons (first alon)
;                           (insert an (rest alon)))])])))
;  (sort (list 2 1 3)))
;=>
;(define (sort_0 alon)
;  (cond
;    [(empty? alon) empty]
;    [(cons? alon) (insert_0 (first alon)
;                            (sort_0 (rest alon)))]))
;(define (insert_0 an alon)
;  (cond
;    [(empty? alon) (cons an empty)]
;    [else (cond
;            [(> an (first alon)) (cons an alon)]
;            [else (cons (first alon)
;                        (insert_0 an (rest alon)))])]))
;(sort_0 (list 2 1 3))
;=>
;(cond
;  [(empty? (list 2 1 3)) empty]
;  [(cons? (list 2 1 3)) (insert_0 (first (list 2 1 3))
;                                  (sort_0 (rest (list 2 1 3))))])
;=> ...

;(equal? (sort (list 1)) (sort (list 2)))
;=>
;(equal?
; (local
;   ((define (sort alon)
;      (cond
;        [(empty? alon) empty]
;        [(cons? alon) (insert (first alon)
;                              (sort (rest alon)))]))
;    (define (insert an alon)
;      (cond
;        [(empty? alon) (cons an empty)]
;        [else (cond
;                [(> an (first alon)) (cons an alon)]
;                [else (cons (first alon)
;                            (insert an (rest alon)))])])))
;   (sort (list 1)))
; (sort (list 2)))
;=>
;(define (sort_0 alon)
;  (cond
;    [(empty? alon) empty]
;    [(cons? alon) (insert_0 (first alon)
;                            (sort_0 (rest alon)))]))
;(define (insert_0 an alon)
;  (cond
;    [(empty? alon) (cons an empty)]
;    [else (cond
;            [(> an (first alon)) (cons an alon)]
;            [else (cons (first alon)
;                        (insert_0 an (rest alon)))])]))
;(equal?
; (sort_0 (list 1))
; (sort (list 2)))
;=>
;(equal?
; (insert_0 (first (list 1))
;           (sort_0 (rest (list 1))))
; (sort (list 2)))
;=>
;(equal?
; (insert_0 1 (sort_0 empty))
; (sort (list 2)))
;=> ...
;(equal?
; (list 1)
; (sort (list 2)))
;=>
;(equal?
; (list 1)
; (local
;   ((define (sort alon)
;      (cond
;        [(empty? alon) empty]
;        [(cons? alon) (insert (first alon)
;                              (sort (rest alon)))]))
;    (define (insert an alon)
;      (cond
;        [(empty? alon) (cons an empty)]
;        [else (cond
;                [(> an (first alon)) (cons an alon)]
;                [else (cons (first alon)
;                            (insert an (rest alon)))])])))
;   (sort (list 2))))
;=>
;(define (sort_1 alon)
;  (cond
;    [(empty? alon) empty]
;    [(cons? alon) (insert_1 (first alon)
;                            (sort_1 (rest alon)))]))
;(define (insert_1 an alon)
;  (cond
;    [(empty? alon) (cons an empty)]
;    [else (cond
;            [(> an (first alon)) (cons an alon)]
;            [else (cons (first alon)
;                        (insert_1 an (rest alon)))])]))
;(equal?
; (list 1)
; (sort_1 (list 2)))
;=>
;(equal?
; (list 1)
; (sort_1 (insert_1 (first (list 2))
;                   (sort_1 (rest (list 2))))))
;=>
;(equal?
; (list 1)
; (insert_1 2 (sort_1 empty)))
;=>...
;(equal? (list 1) (list 2))
;=>
;false

;; ex 18.1.7

;; ex 18.1.8

;; ex 18.1.9

;; ex 18.1.10

(define-struct star (name instrument))
(define alos
  (list (make-star 'Chris 'saxophone)
        (make-star 'Robby 'trumpet)
        (make-star 'Matt 'violin)
        (make-star 'Wen 'guitar)
        (make-star 'Matt 'radio)))

;; last-occurrence : symbol list-of-star -> star or false
(define (last-occurrence s alos)
  (cond
    [(empty? alos) false]
    [else
     (local
       ((define r (last-occurrence s (rest alos))))
       (cond
         [(star? r) r]
         [(symbol=? s (star-name (first alos))) (first alos)]
         [else false]))]))

;; test
(check-expect (last-occurrence 'Matt alos) (make-star 'Matt 'radio))
(check-expect (last-occurrence 'Steve alos) false)
(check-expect (last-occurrence 'Wen alos) (make-star 'Wen 'guitar))

