;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; ch 20.1.1
;; (define (f x) x)

; 1. (cons f empty)
; (list f)
; 2. (f f)
; f
; 3. (cons f (cons 10 (cons (f 10) empty)))
; (list f 10 10)
;
; 함수도 값으로 취급될 수 있기 때문. 나머지 부분은?


;; ch 20.1.2
;(define (f x) (x 10))
; 매개변수가 함수인 경우. 그리고 매개변수는 표현식에 속한다.
;(define (f x) f)
; 함수는 값이 될 수 있다.
;(define (f x y) (x 'a y 'b))
; x는 함수인 매개변수이며, 모든 매개변수는 표현식에 속한다.


;; ch 20.1.3
(define (a-function=? f1 f2)
  (cond [(= (f1 1.2) (f2 1.2))
         (cond [(= (f1 3) (f2 3))
                (cond [(= (f1 -5.7) (f2 -5.7)) true]
                      [else false])]
               [else false])]
        [else false]))

(define (func1 x)
  (+ x 2))
(define (func2 y)
  (+ 2 y))
(define (func3 z)
  (- z 2))

(check-expect (a-function=? func1 func2) true)
(check-expect (a-function=? func1 func3) false)

(define (function=? f1 f2 x)
  (cond 
    [(= (f1 x) (f2 x)) true]
    [else false]))

(check-expect (function=? func1 func2 9) true)
(check-expect (function=? func2 func3 1.9) false)


;; ch 20.2.1
;1. (number  ->  boolean)
; 숫자를 받아 불린이 되는 함수
;2. (boolean symbol  ->  boolean)
; 불린과 기호를 받아 불린이 되는 함수
;3. (number number number  ->  number)
; 숫자 3개를 입력 받아 숫자가 되는 함수
;4. (number  ->  (listof number))
; 숫자를 받아 숫자로 이루어진 리스트가 되는 함수
;5. ((listof number)  ->  boolean)
; 숫자로 이루어진 리스트를 받아 불린이 되는 함수


;; ch 20.2.2
;1. 수 리스트와 함수(두 수를 입력받아 불린 값을 출력)를 입력받아 수 리스트를 출력하는 sort
;; sort : (listof number) (number number -> boolean) -> (listof number)
;2. 함수(수를 입력받아 또 다른 수를 출력)와 수 리스트를 입력받아 수 리스트를 출력하는 map
;; map : (number -> number) (listof number) -> (listof number)
;3. 기호 리스트를 항목으로 갖는 리스트와 함수(기호 리스트를 입력받아 기호를 출력)를 입력받아 기호 리스트를 출력하는 project
;; project : (listof symbol) ((listof symbol) -> symbol) -> (listof symbol)



;; filter1 : (X number -> boolean) (listof X) number -> (listof X)
(define (filter1 rel-op alon t)
  (cond
    [(empty? alon) empty]
    [(rel-op (first alon) t) 
     (cons (first alon)
           (filter1 rel-op (rest alon) t))]
    [else
     (filter1 rel-op (rest alon) t)]))

(define-struct ir (name price))

;; find : (listof IR) symbol  ->  (listof IR)
(define (find aloir t)
  (filter1 eq-ir? aloir t))

;; eq-ir? : IR symbol  ->  boolean
(define (eq-ir? ir p)
  (symbol=? (ir-name ir) p))



;; ex 20.2.3
;; exclude-car : (listof IR) -> (listof IR)
(define (exclude-car aloir)
  (filter1 not-eq-ir? aloir 'car))

;; not-eq-ir? : IR symbol -> boolean
(define (not-eq-ir? ir p)
  (not (symbol=? (ir-name ir) p)))

;; 계약은 요렇게 - filter1 : (IR symbol -> boolean) (listof IR) symbol -> (listof IR)

(define ir1 (list (make-ir 'doll 8) (make-ir 'doll 13) (make-ir 'car 99) (make-ir 'robot 12)))
(check-expect (exclude-car ir1)
              (list (make-ir 'doll 8) (make-ir 'doll 13) (make-ir 'robot 12)))


;; ex 20.2.4
;1. 리스트와 함수(리스트 상의 두 항목을 입력받아 불린 값을 출력)를 입력받아 수 리스트를 출력하는 sort
;; sort : (listof number) (number number -> boolean) -> (listof number)
;; sort : (listof X) (X X -> boolean) -> (listof X)

;2. 함수(리스트 항목들에서 X를 출력)와 리스트를 입력받아 X 리스트를 출력하는 map
;; map : (number -> number) (listof number) -> (listof number)
;; map : (LI -> X) (listof X) -> (listof X)

;3. 리스트를 항목으로 갖는 리스트와 함수(리스트로부터 X를 출력)를 입력받아 X 리스트를 출력하는 project
;; project : (listof symbol) ((listof symbol) -> symbol) -> (listof symbol)
;; project : (listof X) ((listof X) -> X) -> (listof X)