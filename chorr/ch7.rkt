;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct star (last first dob ssn))
(define-struct airplane (kind max-speed max-load price))

;; ex 7.1.1
(check-expect (number? (make-posn 2 3)) false)
(check-expect (number? (+ 12 10)) true)
(check-expect (posn? 23) false)
(check-expect (posn? (make-posn 23 3)) true)
(check-expect (star? (make-posn 23 3)) false)


(define (distance-to-0 a-pixel)
  (cond
    [(number? a-pixel) a-pixel]
    [(posn? a-pixel) (sqrt
		       (+ (sqr (posn-x a-pixel))
			  (sqr (posn-y a-pixel))))]))

(define-struct square (nw length))
(define-struct circle (center radius))

;; perimeter : shape  ->  number
;; to compute the perimeter of a-shape
#| 템플릿
(define (perimeter a-shape)
  (cond
    [(square? a-shape) 
     ... (square-nw a-shape) ... (square-length a-shape) ...]
    [(circle? a-shape)
     ... (circle-center a-shape) ... (circle-radius a-shape) ...]))
|#

(define (perimeter a-shape)
  (cond
    [(square? a-shape) (* (square-length a-shape) 4)]
    [(circle? a-shape) (* (* 2 (circle-radius a-shape)) pi)]))

;; ex 7.1.2
; 테스트:
(check-expect (perimeter (make-square (make-posn 20 20) 3)) 12)
(check-within (perimeter (make-circle (make-posn 10 99) 1)) 6.28 0.01)

;; ex 7.1.3
;; area : shape  ->  number
(define (area a-shape)
  (cond
    [(square? a-shape) (sqr (square-length a-shape))]
    [(circle? a-shape) (* (sqr (circle-radius a-shape)) pi)]))
;; 앞에서 사용한 템플릿을 그대로 적용 가능하다.

; 테스트:
(check-expect (area (make-square (make-posn 20 20) 3)) 9)
(check-within (area (make-circle (make-posn 10 99) 1)) 3.14 0.01)

#|
;; 데이터 정의:
(define-struct circle (center radius))
(define-struct square (nw length))
;; 도형(shape)은 다음 두 구조체 중 하나다.
;; 1. a structure: (make-circle p s)
;;    p는 posn이고 s는 수다.
;; 2. a structure: (make-square p s)
;;    p는 posn이고 s는 수다.

;; 계약, 목적, 헤더: 
;; perimeter : shape  ->  number
;; a-shape의 둘레를 계산함

;; 예: 테스트 참조

;; 템플릿:
;; (define (f a-shape)
;;   (cond
;;     [(square? a-shape)
;;     ... (square-nw a-shape) ... (square-length a-shape) ...]
;;     [(circle? a-shape)
;;     ... (circle-center a-shape) ... (circle-radius a-shape) ...]))

;; 정의: 
(define (perimeter a-shape)
  (cond
    [(circle? a-shape)
     (* (* 2 (circle-radius a-shape)) pi)]
    [(square? a-shape)
     (* (square-length a-shape) 4)]))

;; 테스트: (예와 동일)
(= (perimeter (make-square ... 3)) 12)
(= (perimeter (make-circle ... 1)) (* 2 pi))
|#

;; ex 7.2.1
;; 데이터 정의:
