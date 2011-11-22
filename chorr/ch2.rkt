;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch2) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.ss" "teachpack" "htdp")))))
;; ex 2.1.1
; 제곱 연산
(sqr 4)
; sin 연산
(sin 1)
; 두 수 중에서 큰 수 결정 연산
(max 9 2)

;; ex 2.1.2
(sqrt 4)
(sqrt 2)
(sqrt -1)
; tan 연산 존재한다.
(tan 1)

;; ex 2.2.1
(define (Fahrenheit->Celsius f)
  (- (/ (+ f 40) 1.8) 40))
;(convert-gui Fahrenheit->Celsius)
;(convert-repl Fahrenheit->Celsius)
(convert-file "in.dat" Fahrenheit->Celsius "out.dat")

;; ex 2.2.2
(define (dollar->euro dollar)
  (* dollar 0.729447808))
(dollar->euro 200)

;; ex 2.2.3
(define (triangle a h)
  (/ (* a h) 2))
(triangle 10 5)

;; ex 2.2.4
(define (convert3 x y z)
  (+ (* 100 z) (* 10 y) x))
(convert3 1 2 3)

;; ex 2.2.5
(define (f n)
  (+ (/ n 3) 2))
(f 2)
(f 5)
(f 9)
; 여기서 stepper를 사용해본다.
(define (f-1 n)
  (+ (sqr n) 10))
(define (f-2 n)
  (+ (* (/ 1 2) (sqr n)) 20))
(define (f-3 n)
  (- 2 (/ 1 n)))
(f-1 2)
(f-1 9)
(f-2 2)
(f-2 9)
(f-3 2)
(f-3 9)

;; ex 2.3.1
(define (wage h)
  (* 12 h))
(define (tax total-pay)
  (* total-pay 0.15))
(define (netpay h)
  (- (wage h) (tax (wage h))))
(tax (wage 300)) ; 540

;; ex 2.3.2
(define (sum-coins penny nickel dime quarter)
  (+ penny (* nickel 5) (* dime 10) (* quarter 25)))
(sum-coins 3 4 6 1) ; 108

;; ex 2.3.3
(define (total-profit audience)
  (- (* audience 5) (* audience 0.5) 20))
(total-profit 500) ; 2230

;; ex 2.4.1
;(+ (10) 20)
;(10 + 20)
;function call: expected a function after the open parenthesis, but found a number
;함수 호출: 여는 괄호 다음엔 함수가 와야하지만, 숫자가 있다.
;(+ +)
;+: expected a function call, but there is no open parenthesis before this function
;+: 함수를 호출해야 하는데 이 함수 앞에 여는 괄호가 없다.

;; ex 2.4.2
;(define (f2 1)
;  (+ x 10))
;변수가 있어야하지만 숫자가 있다.
(define (f2 x)
  (+ x 10))

;(define (g x)
;  + x 10)
;함수 본문에는 하나의 표현식만 가능한데, 2 부분이 더 있다.
(define (g x)
  (+ x 10))

;(define h(x)
;  (+ x 10))
;변수명 뒤에는 하나의 표현식만 가능하나, 1 부분이 더 있다.
(define (h x)
  (+ x 10))

;; ex 2.4.3
;(+ 5 (/ 1 0))
;0 으로 나누려 함
;(sin 10 20)
;하나의 인자만 허용
;(somef 10)
;정의되지 않은 함수

;; ex 2.4.4
(define (somef x)
  (sin x x))
;(somef 10 20)
;하나의 인자만 허용
;(somef 10)
;하나의 인자만 허용, (sin x x) 에서 오류
