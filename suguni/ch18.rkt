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

;; ex 18.1.11
(check-expect (last-occurrence 'Matt (list (make-star 'Matt 'violin)
                                           (make-star 'Matt 'radio)))
              (make-star 'Matt 'radio))
;; 2번 발생 (stepper로 확인)

;; ex 18.1.12
;; maxi : non-empty-lon->number
;; alon내에서 가장 큰 수를 찾는다.
(define (maxi alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else (cond
            [(> (first alon) (maxi (rest alon))) (first alon)]
            [else (maxi (rest alon))])]))
;; test
(check-expect (maxi (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 20)

;; maxi-2 : non-empty-long -> number
;; maxi의 local 적용 버전
(define (maxi-2 alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else (local
            ((define r (maxi-2 (rest alon))))
            (cond
              [(> (first alon) r) (first alon)]
              [else r]))]))
;; test
(check-expect (maxi-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 20)
;; (maxi-2 (rest alon))을 한번만 실행하면 되므로 속도가 개선되었다.

;; ex 18.1.13
;; to-blue-eyed-ancestor : ftn -> path or false
;; a-ftn 트리로부터 눈이 파란 조상까지의 경로를 계산한다.
(define (to-blue-eyed-ancestor a-ftn)
  (cond
    [(empty? a-ftn) false]
    [else
     (local
       ((define f (to-blue-eyed-ancestor (child-father a-ftn)))
        (define m (to-blue-eyed-ancestor (child-mother a-ftn))))
       (cond
         [(symbol=? (child-eyes a-ftn) 'blue) empty]
         [(list? f) (cons 'father f)]
         [(list? m) (cons 'mother m)]
         [else false]))]))

;; Data set
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

;; test
(check-expect (to-blue-eyed-ancestor Eva) empty)
(check-expect (to-blue-eyed-ancestor Gustav) (list 'mother))
(check-expect (to-blue-eyed-ancestor Adam) false)
(define Evan (make-child empty empty 'Evan 1987 'red))
(define Hal (make-child Gustav Evan 'Hal 1988 'hazel))
(check-expect (to-blue-eyed-ancestor Hal) (list 'father 'mother))

;; ex 18.1.14
;; 연습문제 15.3.4를 다시 풀어보기

;; p323
;; mult10 : list-of-digits->list-of-numbers
;; alod의 각 숫자에 (expt 10 p)를 곱하여 수 리스트를 만든다.
;; (expt 10 p)에서 p는 뒤에 나오는 숫자의 개수
;(define (mult10 alod)
;  (cond
;    [(empty? alod) empty]
;    [else (cons (* (expt 10 (length (rest alod))) (first alod))
;                (mult10 (rest alod)))]))
;; test
(check-expect (mult10 (list 1 2 3)) (list 100 20 3))

;; mult10 : list-of-digits->list-of-numbers
;; local 적용 버전
(define (mult10 alod)
  (cond
    [(empty? alod) empty]
    [else (local
            ((define a-digit (first alod))
             (define the-rest (rest alod))
             (define p (length the-rest)))
            (cons (* (expt 10 p) a-digit)
                  (mult10 the-rest)))]))
;; 목적
;; 계산 중간값에 이름을 부여하여 명확히 하고자 함
;; 반복된 표현에 이름 부여하여 두번 수행하지 않게 하기

;; ex 18.1.15
;; extract1 : inventory -> inventory
;; an-inv의 모든 항목 중 1달러 미만의 항목들로 이루어진 inventory 리스트를 작성한다.
(define-struct ir (name price))
;(define (extract1 an-inv)
;  (cond
;    [(empty? an-inv) empty]
;    [else
;     (cond
;       [(<= (ir-price (first an-inv)) 1.00)
;        (cons (first an-inv) (extract1 (rest an-inv)))]
;       [else (extract1 (rest an-inv))])]))

(define (extract1 an-inv)
  (cond
    [(empty? an-inv) empty]
    [else (local
            ((define inv (first an-inv))
             (define ext1 (extract1 (rest an-inv))))
            (cond
              [(<= (ir-price inv) 1.00) (cons inv ext1)]
              [else ext1]))]))

(check-expect (extract1 (list (make-ir 'a 1.1) (make-ir 'b 0.9) (make-ir 'c 0.8)))
              (list (make-ir 'b 0.9) (make-ir 'c 0.8)))
