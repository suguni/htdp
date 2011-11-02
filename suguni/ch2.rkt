;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch2) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "convert.ss" "teachpack" "htdp")))))
;; ex 2.1.1
(sqr 10)       ;; square
(sin (/ pi 2)) ;; sin
(> 10 20)      ;; compare

;; ex 2.1.2
(sqrt 4)
(sqrt 2)
(sqrt -1)
(tan (/ pi 4)) ;; tan

;; ex 2.2.1
(define (Fahrenheit->Celsius temp)
  (* (- temp 32) 5/9))
(Fahrenheit->Celsius 10)

;(convert-gui Fahrenheit->Celsius)
;(convert-repl Fahrenheit->Celsius)
;(convert-file "ex2.2.1-input.dat" Fahrenheit->Celsius "ex2.2.1-output.dat")

;; ex 2.2.2 dollar to euro
;; at 2011.10.29, 1 dollar = 0.71 euro
(define (dollar->euro m)
  (* m 0.71))
(dollar->euro 10)

;; ex 2.2.3, compute triangle area
(define (triangle w h)
  (/ (* w h) 2))
(triangle 2 2)

;; ex 2.2.4
(define (convert3 a b c)
  (+ a (* b 10) (* b 100)))
(convert3 3 2 1)

;; ex 2.2.5
(define (f n)
  (+ (/ n 3) 2))
(f 2)
(f 5)
(f 9)

(define (f1 n)
  (+ (sqr n) 10))
(f1 2)
(f1 9)

(define (f2 n)
  (+ (* (sqr n) 1/2) 20))
(f2 2)
(f2 9)

(define (f3 n)
  (- 2 (/ 1 n)))
(f3 2)
(f3 9)

;; ch 2.3
(define (wage h)
  (* 12 h))
(wage 6)

;; ex 2.3.1
(define (tax income)
  (* income 0.15))
(tax 100) ;; 15
(define (netpay h)
  (* h 12))
(netpay 10) ;; 120

;; ex 2.3.2
(define (sum-coins penny nickel dime quater)
  (+ penny (* nickel 5) (* dime 10) (* quater 25)))
(sum-coins 10 2 2 3) ;; 10 + 2*5 + 2*10 + 3*25 = 115

;; ex 2.3.3
(define (total-profit audience)
  (- (* audience 5)
     (+ 20 (* audience 0.5))))
(total-profit 100) ;; 430

;; ch 2.4

;; ex 2.4.1
;; (+ (10) 20)
;; (10 + 20)
;; ERROR> function call: expected a function after the open parenthesis, but found a number
;;        ( 다음에는 함수가 와야 한다

;; (+ +)
;; ERROR> +: expected a function call, but there is no open parenthesis before this function
;;        두 번째 + 는 함수 호출인데, 이 앞에 ( 가 없다.

;; ex 2.4.2

;;(define (f 1)
;;    (+ x 10))
;; ERROR> define: expected a variable, but found a number
;;        변수가 있어야 할 곳에 숫자가 있다. 함수 인자 위치에 숫자가 있다.

;; (define (g x)
;;   + x 10)
;; ERROR> define: expected only one expression for the function body, but found 2 extra parts
;;        함수 몸체에는 하나의 표현식만 올 수 있는데, 2개가 더 있다.

;; (define h(x)
;;   (+ x 10))
;; ERROR> define: expected only one expression after the variable name h, but found 1 extra part
;;        h 변수 다음에 하나의 표현식만 올 수 있는데, 1개가 더 있다. h를 (x)로 정의하려는데, 뒤에 (+ x 10) 이 더 있어서 오류

;; ex 2.4.3
;; (+ 5 (/ 1 0))
;; ERROR> /: division by zero
;;        0 으로 나눌 수 없다.

;; (sin 10 20)
;; ERROR> sin: expects only 1 argument, but found 2
;;        sin 함수는 인자를 1개만 받는데, 2개가 있다.

;; (somef 10)
;; ERROR> somef: this function is not defined
;;        somef 함수가 정의되지 않았다.

;; ex 2.4.4
;; (define (somef x)
;;   (sin x x))

;; (somef 10 20)
;; ERROR> somef: expects only 1 argument, but found 2
;;        somef 함수는 인자를 1개만 받는데, 2개가 있다.
;; (somef 10 20) 이 강조된다.

;; (somef 10)
;; ERROR> sin: expects only 1 argument, but found 2
;;        sin 함수는 인자를 1개만 받는데, 2개가 있다.
;; (sin x x) 이 강조된다.

