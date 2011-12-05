;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch8) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))

;; ex 8.3.1
#| 3.
  (cond
    [(= 2 0) false]
    [(> 2 1) (symbol=? 'a 'a)]
    [else (= (/  1 2) 9)])
= (cond
    [false false]
    [(> 2 1) (symbol=? 'a 'a)]
    [else (= (/  1 2) 9)])
= (cond
    [(> 2 1) (symbol=? 'a 'a)]
    [else (= (/  1 2) 9)])
= (cond
    [true (symbol=? 'a 'a)]
    [else (= (/  1 2) 9)])
= (symbol=? 'a 'a)
= true
|#

;; ex 8.3.2
;; f : number number  ->  number
(define (f x y)
  (+ (* 3 x) (* y y)))

#| 3.
  (f (f 1 (* 2 3)) 19)
= (f (f 1 6) 19)
= (f (+ (* 3 1) (* 6 6)) 19)
= (f (+ 3 36) 19)
= (f 39 19)
= (+ (* 3 39) (* 19 19))
= (+ 117 361)
= 478
|#


;; ex 8.6.3
(define PRICE 5)
(define SALES-TAX (* .08 PRICE))
(define TOTAL (+ PRICE SALES-TAX))

#| 처리 과정
(define PRICE 5)
(define SALES-TAX (* .08 5))
(define TOTAL (+ PRICE SALES-TAX))

(define PRICE 5)
(define SALES-TAX 0.4)
(define TOTAL (+ PRICE SALES-TAX))

(define PRICE 5)
(define SALES-TAX 0.4)
(define TOTAL (+ 5 0.4))

(define PRICE 5)
(define SALES-TAX 0.4)
(define TOTAL 5.4)
|#


;; ex 8.7.1
;(define-struct personnel-record (name salary dob ssn))
;올바름
;(define-struct oops ())
;올바름
;(define-struct child (dob date (- date dob)))
;(- date dob) 연산표현이 쓰임 
;(define-struct (child person) (dob date))
;괄호 문법 오류 
;(define-struct child (parents dob date))
;올바름

;; ex 8.7.2
;1. (make-point 1 2 3)
;2. (make-point (make-point 1 2 3) 4 5)
;3. (make-point (+ 1 2) 3 4)
;3개 모두 값

;; ex 8.7.3
(define-struct ball (x y speed-x speed-y))

(number? (make-ball 1 2 3 4))
;> false
(ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3))
;> 3
(ball-y (make-ball (+ 1 2) (+ 3 3) 2 3))
;> 6

;(number? (make-ball 1 3 4))
;(ball-x (make-posn 1 2))
;(ball-speed-y 5)
;모두 오류 표현
