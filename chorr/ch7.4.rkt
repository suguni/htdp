;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch7.4) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;; ex 7.4.1

;; 데이터 정의:
(define-struct circle (center radius color))
;; 원(circle)은 구조체다.
;; (make-circle p s c)
;; p는 posn이고 s는 수이고, c는 기호이다.

;; 데이터 정의:
(define-struct rectangle (position width height color))
;; 직사각형(rectangle)은 구조체다.
;; (make-rectangle p w h c)
;; p는 posn이고 w, h는 수이고, c는 기호이다.

;; 도형(shape)는 다음 두 구조체 중 하나다.
;; 1. 원(circle)
;; 2. 직사각형(rectangle)

;; 템플릿:
;; fun-for-shape : shape -> ???
#|
(define (fun-for-shape a-shape)
  (cond
    [(circle? a-shape) ...]
    [(rectangle? a-shape) ...]))
|#


;; ex 7.4.2
;; draw-shape : shape -> boolean
(define (draw-shape a-shape)
  (cond
    [(circle? a-shape) (draw-a-circle a-shape)]
    [(rectangle? a-shape) (draw-a-rectangle a-shape)]))

;; draw-a-circle : circle -> boolean
(define (draw-a-circle c)
  (draw-circle (circle-center c)
               (circle-radius c)
               (circle-color c)))

;; draw-a-rectangle : rectangle -> boolean
(define (draw-a-rectangle r)
  (draw-solid-rect (rectangle-position r)
                   (rectangle-width r)
                   (rectangle-height r)
                   (rectangle-color r)))

;; ex 7.4.3
;; translate-shape : shape number -> shape
(define (translate-shape a-shape delta)
  (cond
    [(circle? a-shape) (translate-circle a-shape delta)]
    [(rectangle? a-shape) (translate-rectangle a-shape delta)]))

;; translate-circle : circle number -> circle
(define (translate-circle c delta)
  (make-circle 
   (make-posn (+ (posn-x (circle-center c)) delta)
              (posn-y (circle-center c)))
   (circle-radius c)
   (circle-color c)))

;; translate-rectangle : rectangle number -> rectangle
(define (translate-rectangle r delta)
  (make-rectangle 
   (make-posn (+ (posn-x (rectangle-position r)) delta)
              (posn-y (rectangle-position r)))
   (rectangle-width r)
   (rectangle-height r)
   (rectangle-color r)))

;; ex 7.4.4
;; clear-shape : shape -> boolean
(define (clear-shape a-shape)
  (cond
    [(circle? a-shape) (clear-a-circle a-shape)]
    [(rectangle? a-shape) (clear-a-rectangle a-shape)]))

;; clear-a-circle : circle -> boolean
(define (clear-a-circle c)
  (clear-circle (circle-center c)
                (circle-radius c)
                (circle-color c)))

;; clear-a-rectangle : rectangle -> boolean
(define (clear-a-rectangle r)
  (clear-solid-rect (rectangle-position r)
                    (rectangle-width r)
                    (rectangle-height r)
                    (rectangle-color r)))

;; ex 7.4.5
;; draw-and-clear-shape : shape -> boolean
(define (draw-and-clear-shape a-shape)
  (and (draw-shape a-shape)
       (sleep-for-a-while 0.5)
       (clear-shape a-shape)))

;; ex 7.4.6
;; move-shape : number shape -> shape
(define (move-shape delta a-shape)
  (cond
    [(draw-and-clear-shape a-shape) (translate-shape a-shape delta)]
    [else a-shape]))


#| 실행
(start 200 200)
(define circle-1 (make-circle (make-posn 40 50) 20 'red))
(define rect-1 (make-rectangle (make-posn 40 140) 30 20 'blue))
(draw-shape (move-shape 10 (move-shape 10 (move-shape 20 (move-shape 20 circle-1)))))
(draw-shape (move-shape 40 (move-shape 30 (move-shape 20 (move-shape 10 rect-1)))))
|#

