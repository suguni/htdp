;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch20) (read-case-sensitive #t) (teachpacks ((lib "dir.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.ss" "teachpack" "htdp")))))
;; 20. 함수는 값이다.

(define (filter1 rel-op lon t)
  (cond
    [(empty? lon) empty]
    [else
     (cond
       [(rel-op (first lon) t) (cons (first lon) 
                                     (filter1 rel-op (rest lon) t))]
       [else (filter1 rel-op (rest lon) t)])]))

;; ex 20.1.1
#|
(define (f x) x)

(cons f empty)
=> 값, (list f)

(f f)
=> 값, 함수 f

(cons f (cons 10 (cons (f 10) empty)))
=> (list f 10 10)

함수 이름과 기본 연산 또한 값이라고 했으니 위 모두는 값이다.
|#

;; ex 20.1.2
#|
(define (f x) (x 10))
인자 x가 함수인 경우 올바르다.

(define (f x) f)
자기 자신(함수)을 반환하므로 올바르다.

(define (f x y) (x 'a y 'b))
인자 x가 함수인 경우 'a y 'b를 인자로 x를 실행한 결과를 반환하므로 올바르다.
|#

;; ex 20.1.3
;; a-function=? : function function -> boolean
(define (a-function=? fun1 fun2)
  (and (eq? (fun1 1.2) (fun2 1.2))
       (eq? (fun1 3) (fun2 3))
       (eq? (fun1 -5.7) (fun2 -5.7))))

;; function=? : number function function -> boolean
;; n에 대한 f1의 결과와 f2의 결과가 동일한지 검사한다.
(define (function=? n f1 f2)
  (eq? (f1 n) (f2 n)))

;; ex 20.2.1
#|
1. (number -> boolean)
수를 입력받아 불린값을 출력하는 함수

2. (boolean symbol -> boolean)
불린값과 심볼을 입력받아 불린값을 출력하는 함수

3. (number number number -> number)
수 3개를 입력받아 수를 출력하는 함수

4. (number -> (listof number))
수를 입력받아 수 리스트를 출력하는 함수

5. ((listof number) -> boolean)
수 리스트를 입력받아 불린값을 출력하는 함수
|#

;; ex 20.2.2
;; 1.
;; sort : (listof number) (number number -> boolean) -> (listof number)

;; 2.
;; map : (number -> number) (listof number) -> (listof number)

;; 3.
;; project : (listof (listof symbol)) ((listof symbol) -> symbol) -> (listof symbol)

;; ex 20.2.3
;; find-not-car : (listof symbol) -> (listof symbol)
(define (find-not-car los)
  (filter1 not-eq? los 'car))

;; not-eq? : symbol symbol -> boolean
(define (not-eq? v1 v2)
  (not (eq? v1 v2)))

;; filter1 : (symbol symbol -> boolean) (listof symbol) symbol -> (listof symbol)

;; ex 20.2.4
;; 1.
;; sort : (listof I) (I I -> boolean) -> (listof I)

;; 2.
;; map : (I -> X) (listof I) -> (listof X)

;; 3.
;; project : (listof (listof I)) ((listof I) -> X) -> (listof X)
