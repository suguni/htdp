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
(define-struct spider (remain-legs need-space))
(define-struct elephant (need-space))
(define-struct monkey (intelligence need-space))
;; 동물(animal)은 다음 세 구조체 중 하나다.
;; 1. (make-spider l s)
;;    l, s는 수다.
;; 2. (make-elephant s)
;;    s는 수다.
;; 3. (make-monkey i s)
;;    i, s는 수다.

;; 템플릿:
;; (define (f a-animal)
;;   (cond
;;     [(spider? a-animal)
;;      ... (spider-remain-legs a-animal) ... (spider-need-space a-animal) ...]
;;     [(elephant? a-animal)
;;      ... (elephant-need-space a-animal) ...]
;;     [(monkey? a-animal)
;;      ... (monkey-intelligance a-animal) ... (monkey-need-space a-animal) ...]))

;; ex 7.2.2
;; 위 문제와 같은 형태



;; -- ch 7.3
#|
;; 데이터 정의:
(define-struct circle (center radius))
;; 원(circle)은 구조체다.
;; (make-circle p s)
;; p는 posn이고 s는 수다.

(define-struct square (nw length))
;; 정사각형(square)은 구조체다.
;; (make-square p s)
;; p는 posn이고 s는 수다.

;; 도형(shape)은 다음 두 구조체 중 하나다.
;; 1. circle
;; 2. square

;; 최종 정의: 
;; perimeter : shape -> number
(define (perimeter a-shape)
  (cond
    [(circle? a-shape) (perimeter-circle a-shape)]
    [(square? a-shape) (perimeter-square a-shape)]))

;; perimeter-circle : circle -> number
(define (perimeter-circle a-circle)
  (* (* 2 (circle-radius a-circle)) pi))

;; perimeter-square : square -> number
(define (perimeter-square a-square)
  (* (square-length a-square) 4))
|#


;; ex 7.3.1
#|
;; 데이터 정의: (추가)
(define-struct rectangle (position width height))
;; 직사각형(rectangle)은 구조체다.
;; (make-rectangle p w h)
;; p는 posn이고 w, h는 수다.

;; 최종 정의: (수정)
;; perimeter : shape -> number
(define (perimeter a-shape)
  (cond
    [(circle? a-shape) (perimeter-circle a-shape)]
    [(square? a-shape) (perimeter-square a-shape)]
    [(rectangle? a-shape) (perimeter-rectangle a-shape)]))

;; perimeter-rectangle : rectangle -> number
(define (perimeter-rectangle a-rectangle)
  (+ (* (rectangle-width a-rectangle) 2) 
     (* (rectangle-height a-rectangle) 2)))
|#
