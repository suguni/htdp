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

;; ex 6.6.6

;; Contract:
;; draw-and-clear-circle : circle -> boolean

;; Purpose:
;; Draws a circle, waits for a short time, and clears it.

;; Examples:
;; (draw-and-clear-circle a-circle)

;; Definition:
(define (draw-and-clear-circle c)
  (and
   (draw-a-circle c)
   (sleep-for-a-while 0.01)
   (clear-a-circle c)))

;; move-circle : number circle -> circle
;; to draw and clear a circle, translate it by delta pixels
(define (move-circle delta a-circle)
  (cond
    [(draw-and-clear-circle a-circle) (translate-circle a-circle delta)]
    [else a-circle]))

;; Examples:
;; (define c2 (make-circle (make-posn 200 200) 10 'red))
;; (draw-a-circle 
;;  (move-circle 10
;;               (move-circle 10
;;                            (move-circle 10
;;                                         (move-circle 10 c2)))))

;; ex 6.6.7

;; Data Analysis & Definitions:
(define-struct rectangle (corner width height color))
;; A rect is a structure:
;; (make-rectangle p w h c) where p is a posn, w and h are numbers, c is a symbol.

;; Template:
;; (define (fun-for-rect a-rect)
;;   ... (rectangle-corner a-rect) ...
;;   ... (rectangle-width a-rect) ...
;;   ... (rectangle-height a-rect) ...
;;   ... (rectangle-color a-rect) ...)

;; ex 6.6.8

;; Contract:
;; draw-a-rectangle : rectangle -> None

;; Examples:
;; (start 300 300)
;; (draw-a-rectangle (make-rectangle (make-posn 100 100) 50 100 'red))
;; (stop)

;; Definition:
(define (draw-a-rectangle r)
  (draw-solid-rect (rectangle-corner r)
                   (rectangle-width r)
                   (rectangle-height r)
                   (rectangle-color r)))

;; Tests:
;; (start 300 300)
;; (draw-a-rectangle (make-rectangle (make-posn 50 50) 200 50 'red))
;; (end)

;; ex 6.6.9

;; Contract:
;; in-rectangle? : rectangle posn -> boolean

;; Purpose:
;; posn이 rectangle내에 포함되는지를 검사한다.

;; Examples:
;; (in-rectangle? (make-rectangle (make-posn 2 3) 3 2 'red)
;;                (make-posn 8 6))
;; = false

;; (in-rectangle? (make-rectangle (make-posn 2 3) 3 2 'red)
;;                (make-posn 4 4))
;; = true

;; Definition:
(define (in-rectangle? r p)
  (and (< (posn-x (rectangle-corner r)) (posn-x p))
       (< (posn-x p) (+ (posn-x (rectangle-corner r)) (rectangle-width r)))
       (< (posn-y (rectangle-corner r)) (posn-y p))
       (< (posn-y p) (+ (posn-y (rectangle-corner r)) (rectangle-height r)))))

;; Tests
(in-rectangle? (make-rectangle (make-posn 2 3) 3 2 'red)
               (make-posn 8 6)) ;; = false

(in-rectangle? (make-rectangle (make-posn 2 3) 3 2 'red)
               (make-posn 4 4)) ;; = true

;; ex 6.6.10

;; Contract:
;; translate-rectangle : rectangle number -> rectangle

;; Purpose:
;; 주어진 사각형의 좌상단 점에서 x축이 delta 만큼 떨어진 곳에 있는 사각형의 구조체를 반환한다.

;; Examples:
;; (translate-rectangle (make-rectangle (make-posn 10 10) 5 5 'red) 10)
;; =
;; (make-rectangle (make-posn 20 10) 5 5 'red)

;; Definition:
(define (translate-rectangle r delta)
  (make-rectangle
   (make-posn (+ (posn-x (rectangle-corner r)) delta)
              (posn-y (rectangle-corner r)))
   (rectangle-width r)
   (rectangle-height r)
   (rectangle-color r)))

;; Tests:
(translate-rectangle (make-rectangle (make-posn 10 10) 5 5 'red) 10)
;; =
(make-rectangle (make-posn 20 10) 5 5 'red)

;; ex 6.6.11
;; Contract:
;; clear-a-rectangle : rectangle -> true

;; Examples:
;; (start 300 300)
;; (draw-a-rectangle (make-rectangle (make-posn 100 100) 50 50 'red))
;; (clear-a-rectangle (make-rectangle (make-posn 100 100) 50 50 'red))
;; (stop)

;; Definition:
(define (clear-a-rectangle r)
  (clear-solid-rect (rectangle-corner r)
                    (rectangle-width r)
                    (rectangle-height r)
                    (rectangle-color r)))

;; ex 6.6.12

;; move-rectangle : number rectangle -> rectangle
;; to draw and clear a rectangle, translate it by delta pixels
(define (move-rectangle delta a-rectangle)
  (cond
    [(draw-and-clear-rectangle a-rectangle) (translate-rectangle a-rectangle delta)]
    [else a-rectangle]))

;; Contract:
;; draw-and-clear-rectangle : rectangle -> boolean

;; Purpose:
;; Draws a rectangle, waits for a short time, and clears it.

;; Examples:
;; (draw-and-clear-rectangle a-rectangle)

;; Definition:
(define (draw-and-clear-rectangle r)
  (and
   (draw-a-rectangle r)
   (sleep-for-a-while 0.01)
   (clear-a-rectangle r)))

;; Examples:
;; (define r1 (make-rectangle (make-posn 200 200) 10 10 'red))
;; (draw-a-rectangle 
;;  (move-rectangle 10
;;               (move-rectangle 10
;;                            (move-rectangle 10
;;                                         (move-rectangle 10 r1)))))
