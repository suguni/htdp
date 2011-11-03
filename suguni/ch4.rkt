;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "convert.ss" "teachpack" "htdp")))))
;; 4장. 조건문과 함수

;; ch 4.1

;; ex 4.1.1
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
