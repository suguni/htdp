;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6.6) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;; ch 6.6 Moving Circles and Rectangles

;; ex 6.6.1

;; Data Analysis & Definitions:
(define-struct circle (center radius color))
;; A circle is a structure:
;; (make-circle p r c) where p is a posn, r is a number, c is a symbol.

;; Template:
;; (define (fun-for-circle a-circle)
;;   ... (circle-center a-circle) ...
;;   ... (circle-radius a-circle) ...
;;   ... (circle-color a-circle) ...)

;; ex 6.6.2

;; Contract:
;; draw-a-circle : circle -> true

;; Examples:
;; (start 300 300)
;; (draw-a-circle (make-circle (make-posn 100 100) 50 'red))
;; (stop)

;; Definition:
(define (draw-a-circle c)
  (draw-circle (circle-center c)
               (circle-radius c)
               (circle-color c)))

;; ex 6.6.3

;; Contract:
;; in-circle? : circle posn -> boolean

;; Purpose:
;; posn이 circle내에 포함되는지를 검사한다.

;; Examples:
;; (in-circle? (make-circle (make-posn 6 2) 1 'red)
;;             (make-posn 8 6))
;; = false

;; (in-circle? (make-circle (make-posn 6 2) 1 'red)
;;             (make-posn 6 1.5))
;; = true

;; Auxiliary function
(define (distance p1 p2)
  (sqrt
   (+ (sqr (- (posn-x p1) (posn-x p2)))
      (sqr (- (posn-y p1) (posn-y p2))))))

;; Definition:
(define (in-circle? c p)
  (< (distance (circle-center c) p)
     (circle-radius c)))

;; Tests
(in-circle? (make-circle (make-posn 6 2) 1 'red)
            (make-posn 8 6)) ;; = false

(in-circle? (make-circle (make-posn 6 2) 1 'red)
            (make-posn 6 1.5)) ;; = true

;; ex 6.6.4

;; Contract:
;; translate-circle : circle number -> circle

;; Purpose:
;; 주어진 원의 중심에서 delta 만큼 떨어진 곳에 있는 원의 구조체를 반환한다.

;; Examples:
;; (translate-circle (make-circle (make-posn 10 10) 5 'red) 10)
;; =
;; (make-circle (make-posn 20 10) 5 'red)

;; Definition:
(define (translate-circle c delta)
  (make-circle
   (make-posn (+ (posn-x (circle-center c)) delta)
              (posn-y (circle-center c)))
   (circle-radius c)
   (circle-color c)))

;; Tests:
(translate-circle (make-circle (make-posn 10 10) 5 'red) 10)
;; =
(make-circle (make-posn 20 10) 5 'red)

;; ex 6.6.5

;; Contract:
;; clear-a-circle : circle -> true

;; Examples:
;; (start 300 300)
;; (draw-a-circle (make-circle (make-posn 100 100) 50 'red))
;; (clear-a-circle (make-circle (make-posn 100 100) 50 'red))
;; (stop)

;; Definition:
(define (clear-a-circle c)
  (clear-circle (circle-center c)
                (circle-radius c)
                (circle-color c)))

;; 