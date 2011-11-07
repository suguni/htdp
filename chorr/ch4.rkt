;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.ss" "teachpack" "htdp")))))
;; ex 4.1.1
(and (> 4 3) (<= 10 100)) ; true
(or (> 4 3) (= 10 100)) ; true
(not (= 2 3)) ; true

;; ex 4.1.2
; x = 4
; 1. true
; 2. false
; 3. false
; x = 2
; 1. false
; 2. false
; 3. false
; x = 7/2
; 1. true
; 2. true
; 3. false

;; ex 4.2.1
(define (is-between-cond-1? n)
  (and (> n 3) (<= n 7)))
(define (is-between-cond-2? n)
  (and (>= n 3) (<= n 7)))
(define (is-between-cond-3? n)
  (and (>= n 3) (< n 9)))
(define (is-between-cond-4? n)
  (or (and (> n 1) (< n 3))
      (and (> n 9) (< n 11))))
(define (is-between-cond-5? n)
  (not (and (>= n 1) (<= n 3))))

;; ex 4.2.2
; in-interval-1? : number -> boolean
; -3 초과이며 0 미만일 경우 참
(define (in-interval-1? x)
  (and (< -3 x) (< x 0)))
; in-interval-2? : number -> boolean
; 1 미만인 수 또는 2 초과인 수
(define (in-interval-2? x)
  (or (< x 1) (> x 2)))
; in-interval-3? : number -> boolean
; 1 미만인 수 또는 5 초과인 수
(define (in-interval-3? x)
  (not (and (<= 1 x) (<= x 5))))

(in-interval-1? -2) ; true
(in-interval-2? -2) ; true
(in-interval-3? -2) ; true

;; ex 4.2.3
(define (equation-1 n)
  (= (+ (* 4 n) 2) 62))
(define (equation-2 n)
  (= (* 2 n n) 102))
(define (equation-3 n)
  (= (+ (* 4 n n) (* 6 n) 2) 462))
(equation-1 10) ; false
(equation-1 12) ; false
(equation-1 14) ; false
(equation-2 10) ; false
(equation-2 12) ; false
(equation-2 14) ; false
(equation-3 10) ; true
(equation-3 12) ; false
(equation-3 14) ; false

;; ex 4.2.4
; 2.2.1
(define (Fahrenheit->Celsius f)
  (- (/ (+ f 40) 1.8) 40))
(= (Fahrenheit->Celsius 32) 0)
; 2.2.2
(define (dollar->euro dollar)
  (* dollar 0.8))
(= (dollar->euro 200) 160)
; 2.2.3
(define (triangle a h)
  (/ (* a h) 2))
(= (triangle 10 5) 25)
; 2.2.4
(define (convert3 x y z)
  (+ (* 100 z) (* 10 y) x))
(= (convert3 1 2 3) 321)

;; ex 4.3.1
;(cond
;  [(< n 10) 20]
;  [(> n 20) 0]
;  [else 1])
;정상
;(cond
;  [(< n 10) 20]
;  [(and (> n 20) (<= n 30))]
;  [else 1])
;[질문 답]이 매칭되지 않는다.답이 없음.
;(cond [(< n 10) 20]
;      [* 10 n]
;      [else 555]);
;[질문 답]이 매칭되지 않는다.질문이 없음.

;; ex 4.3.2
(define (find-cond-1 n)
  (cond
    [(<= n 1000) .040]
    [(<= n 5000) .045]
    [(<= n 10000) .055]
    [(> n 10000) .060]))
(find-cond-1 500) ; 0.04
(find-cond-1 2800) ; 0.045
(find-cond-1 15000) ; 0.06

;; ex 4.3.3
(define (find-cond-2 n)
  (cond
    [(<= n 1000) (* .040 1000)]
    [(<= n 5000) (+ (* 1000 .040) 
                    (* (- n 1000) .045))]
    [else (+ (* 1000 .040) 
             (* 4000 .045)
             (* (- n 10000) .055))]))
(find-cond-2 500) ; 40
(find-cond-2 2800) ; 121
(find-cond-2 15000) ; 495

;; ex 4.4.1
(define (interest amount)
  (cond
    [(<= amount 1000) (+ amount (* amount 0.04))]
    [(<= amount 5000) (+ amount (* amount 0.045))]
    [(> amount 5000) (+ amount (* amount 0.05))]))

;; ex 4.4.2
(define (tax income)
  (cond
    [(<= income 240) 0]
    [(<= income 480) 0.15]
    [(> income 480) 0.28]))
(define (taxpay h)
  (* (total-income h) (tax (total-income h))))
(define (total-income h)
  (* h 12))
(define (netpay h)
  (+ (total-income h) (taxpay h)))

;; ex 4.4.3
