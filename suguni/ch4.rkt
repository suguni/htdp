;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; ch4 
; ch 4.1
; ex 4.1.1
(and (> 4 3) (<= 10 100)) ;; true
(or (> 4 3) (= 10 100))   ;; true
(not (= 2 3))             ;; true

;; ex 4.1.2
;; (> x 3)
;; x = 4   -> true
;; x = 2   -> false
;; x = 7/2 -> true

;; (and (> 4 x) (> x 3))
;; x = 4   -> false
;; x = 2   -> false
;; x = 7/2 -> true

;; (= (* x x) x)
;; x = 4   -> false
;; x = 2   -> false
;; x = 7/2 -> false

;; ch 4.2
;; is-5? : number->boolean
(define (is-5 n)
  (= n 5))

;; ex 4.2.1
(define (is-between-1? x)
  (and (< 3 x) (>= 7 x)))
(is-between-1? 4) ;; true
(is-between-1? 9) ;; false

(define (is-between-2? x)
  (and (<= 3 x) (>= 7 x)))
(is-between-2? 3) ;; true
(is-between-2? 8) ;; false

(define (is-between-3? x)
  (and (<= 3 x) (> 9 x)))
(is-between-3? 3) ;; true
(is-between-3? 9) ;; false

(define (is-between-4? x)
  (or (and (< 1 x) (> 3 x))
      (and (< 9 x) (> 11 x))))
(is-between-4? 2) ;; true
(is-between-4? 10) ;; true

(define (is-between-5? x)
  (not (and (<= 1 x) (>= 3 x))))
(is-between-5? 1) ;; false
(is-between-5? 4) ;; true

;; ex 4.2.2

;; 1.
;; in-interval-1? : number -> boolean
;; -3과 0 사이(경계 미포함)인지 검사
(define (in-interval-1? x)
  (and (< -3 x) (< x 0)))

;; 2.
;; in-interval-2? : number -> boolean
;; 1보다 작거(미만)나 2보다 큰지(초과) 검사
(define (in-interval-2? x)
  (or (< x 1) (> x 2)))
  
;; 3.
;; in-interval-3? : number -> boolean
;; 1이상 5이하 범위가 아닌지 검사 - 즉, 1 미만이거나 5초과 인지 검사
(define (in-interval-3? x)
  (not (and (<= 1 x) (<= x 5))))
  
(in-interval-1? -2) ;; => true
(in-interval-2? -2) ;; => true
(in-interval-3? -2) ;; => true

;; ex 4.2.3
;; equation1 : number -> boolean
;; x가 x^2 + 2x + 1 = 의 해인지 판별한다.
(define (equation1 x)
  (= (+ (* x x) (* 2 x) 1) 0))
(equation1 -1) ;; => true
(equation1 1)  ;; => false

(define (equation2 n)
  (= (+ (* 4 n) 2) 62))

(define (equation3 n)
  (= (* 2 n n) 102))

(define (equation4 n)
  (= (+ (* 4 n n) (* 6 n) 2) 462))

'equation2
(equation2 10)
(equation2 12)
(equation2 14)

'equation3
(equation3 10)
(equation3 12)
(equation3 14)

'equation4
(equation4 10)
(equation4 12)
(equation4 14)

;; ex 4.2.4

'claim-Fahrenheit->Celsius
(define (Fahrenheit->Celsius temp)
  (* (- temp 32) 5/9))
(= (Fahrenheit->Celsius 32)
   0)

'claim-dollar-euro
(define (dollar->euro m)
  (* m 0.71))
(= (dollar->euro 10)
   7.1)

'claim-triangle
(define (triangle w h)
  (/ (* w h) 2))
(= (triangle 2 2)
   2)
'claim-convert3
(define (convert3 a b c)
  (+ a (* b 10) (* c 100)))
(= (convert3 1 2 3)
   321)

;; 4.3
;; ex 4.3.1
;; 첫번째 - 올바르다.
;; 두번째 - 잘못되었다. 두번째 조건에 결과값이 없다.
;; 세번째 - 잘못되었다. 두번째 조건에 조건식(비교문)이 없다.

;; ex 4.3.2
'ex4-3-2
(define (ex4-3-2 n)
  (cond
    [(<= n 1000)  .040]
    [(<= n 5000)  .045]
    [(<= n 10000) .055]
    [(> n 10000)  .060]))
(ex4-3-2 500)   ;; .040
(ex4-3-2 2800)  ;; .045
(ex4-3-2 15000) ;; .060

;; ex4.3.3
'ex4-3-3
(define (ex4-3-3 n)
  (cond
    [(<= n 1000) (* .040 1000)]
    [(<= n 5000) (+ (* 1000 .040)
                    (* (- n 1000) .045))]
    [else (+ (* 1000 .040)
             (* 4000 .045)
             (* (- n 10000) .055))]))
(ex4-3-3 500)
(ex4-3-3 2800)
(ex4-3-3 15000)

;; ch 4.4
;; ex 4.4.1
(define (interest amount)
  (+ amount 
     (cond
       [(<= amount 1000) (* amount .040)]
       [(<= amount 5000) (* amount .045)]
       [else (* amount .050)])))
'ex4-4-1-interest
(= (interest 1000)
   1040)
(= (interest 5000)
   5225)
(= (interest 10000)
   10500)

;; ex 4.4.2
(define (tax i)
  (cond
    [(<= i 240) .000]
    [(<= i 480) .150]
    [else .280]))

(define (income h)
  (* h 12))

(define (netpay h)
  (- (income h)
     (tax (income h))))

'ex4-4-2-netpay
(netpay 20) ;; 240
(netpay 40) ;; 479.85

;; ex 4.4.3
(define (pay-back a)
  (cond
    [(<= a 500)  (* a .0025)]
    [(<= a 1500) (+ (* 500 0.0025)
                    (* (- a 500) .0050))]
    [(<= a 2500) (+ (* 500 0.0025)
                    (* 1000 0.0050)
                    (* (- a 1500) .0075))]
    [else        (+ (* 500 0.0025)
                    (* 1000 0.0050)
                    (* 1000 0.0075)
                    (* (- a 2500) .0100))]))
'ex4.4.3-payback
(= (pay-back 400) 1)
(= (pay-back 1400)  5.75)
;; 2000 : 500->1.25, 1000->5, 500->3.75 => 10
(= (pay-back 2000) 10)
;; 2600 : 500->1.25, 1000->5, 1000->7.5, 100->1 => 14.75
(= (pay-back 2600) 14.75)

;; ex 4.4.4
(define (how-many a b c)
  (cond
    [(> (* b b) (* 4 a c)) 2]
    [(= (* b b) (* 4 a c)) 1]
    [(< (* b b) (* 4 a c)) 0]))
'how-many
(= (how-many 1 0 -1) 2)
(= (how-many 2 4 2) 1)
;; 올바르지 않은 2차방정식
(= (how-many 0 2 1) 1) ;; => false

;; changed version
(define (how-many-2 a b c)
  (cond
    [(and (= a 0) (= b 0)) -1] ;; 방정식 아님
    [(= a 0) 1]                ;; bx + c 이므로 항상 1개
    [(> (* b b) (* 4 a c)) 2]
    [(= (* b b) (* 4 a c)) 1]
    [(< (* b b) (* 4 a c)) 0]))
'how-many-2
(= (how-many-2 1 0 -1) 2)
(= (how-many-2 2 4 2) 1)
(= (how-many-2 0 2 1) 1)
