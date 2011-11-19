;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch7.4) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;; ch 7.4

;; ex 7.4.1

;; Data Definitions:
(define-struct circle (center radius color))
;; A circle is a structure:
;;          (make-circle p s c)
;;    where p is a posn, s is a number and c is a color;

(define-struct rectangle (nw width height color))
;; A rectangle is a structure:
;;          (make-rectangle p w h c)
;;    where p is a posn, w and h a number and c is a color.

(define-struct square (nw length color))
;; A square is a structure:
;;          (make-square p l c)
;;    where p is a posn, l is a number and c is a color.

;; fun-for-shape : a-shape -> ???
(define (fun-for-shape a-shape)
  (cond
    [(circle? a-shape) ...]
    [(rectangle? a-shape) ...]
    [(square? a-shape) ...]))

;; ex 7.4.2
;; draw-shape : shape -> true
;; draw a shape s
(define (draw-shape s)
  (cond
    [(circle? s) (draw-a-circle s)]
    [(rectangle? s) (draw-a-rectangle s)]
    [(square? s) (draw-a-square s)]))

;; draw-a-circle : circle -> true
;; draw a circle s
(define (draw-a-circle s)
  (draw-circle (circle-center s)
               (circle-radius s)
               (circle-color s)))

;; draw-a-rectangle : rectangle -> true
;; draw a rectangle s
(define (draw-a-rectangle s)
  (draw-solid-rect (rectangle-nw s)
                   (rectangle-width s)
                   (rectangle-height s)
                   (rectangle-color s)))

;; draw-a-square : square -> true
;; draw a square s
(define (draw-a-square s)
  (draw-solid-rect (square-nw s)
                   (square-length s )
                   (square-length s)
                   (square-color s)))

;; ex 7.4.3
;; translate-shape : shape, number -> shape
(define (translate-shape delta a-shape)
  (cond
    [(circle? a-shape) (translate-circle delta a-shape)]
    [(rectangle? a-shape) (translate-rectangle delta a-shape)]
    [(square? a-shape) (translate-square delta a-shape)]))

;; translate-circle : circle, number -> shape
(define (translate-circle d s)
  (make-circle
   (make-posn (+ d  (posn-x (circle-center s)))
              (posn-y (circle-center s)))
   (circle-radius s)
   (circle-color s)))

;; translate-rectangle : rectangle, number -> shape
(define (translate-rectangle d s)
  (make-rectangle
   (make-posn (+ d (posn-x (rectangle-nw s)))
              (posn-y (rectangle-nw s)))
   (rectangle-width s)
   (rectangle-height s)
   (rectangle-color s)))

;; translate-square : square, number -> shape
(define (translate-square d s)
  (make-square
   (make-posn (+ d (posn-x (square-nw s)))
              (posn-y (square-nw s)))
   (square-length s)
   (square-color s)))

;; ex 7.4.4
;; clear-shape : shape -> true
;; clear a shape s
(define (clear-shape s)
  (cond
    [(circle? s) (clear-a-circle s)]
    [(rectangle? s) (clear-a-rectangle s)]
    [(square? s) (clear-a-square s)]))

;; clear-a-circle : circle -> true
(define (clear-a-circle s)
  (clear-circle (circle-center s)
                (circle-radius s)
                (circle-color s)))

;; draw-a-rectangle : rectangle -> true
(define (clear-a-rectangle s)
  (clear-solid-rect (rectangle-nw s)
                    (rectangle-width s)
                    (rectangle-height s)
                    (rectangle-color s)))

;; clear-a-square : square -> true
(define (clear-a-square s)
  (clear-solid-rect (square-nw s)
                    (square-length s )
                    (square-length s)
                    (square-color s)))

;; tests:
(define ci (make-circle (make-posn 150 150) 80 'red))
(define re (make-rectangle (make-posn 50 50) 200 100 'blue))
(define sq (make-square (make-posn 50 150) 60 'cyan))
;(start 500 500)
;(draw-shape s)
;(draw-shape sq)
;(clear-shape sq)
;(clear-shape s)

;; ex 7.4.5
(define (draw-and-clear-shape a-shape)
  (cond
    [(circle? a-shape) (draw-and-clear-circle a-shape)]
    [(rectangle? a-shape) (draw-and-clear-rectangle a-shape)]
    [(square? a-shape) (draw-and-clear-square a-shape)]))

(define (draw-and-clear-circle s)
  (and
   (draw-a-circle s)
   (sleep-for-a-while 0.01)
   (clear-a-circle s)))

(define (draw-and-clear-rectangle s)
  (and
   (draw-a-rectangle s)
   (sleep-for-a-while 0.01)
   (clear-a-rectangle s)))

(define (draw-and-clear-square s)
  (and
   (draw-a-square s)
   (sleep-for-a-while 0.01)
   (clear-a-square s)))

;; ex 7.4.6
(define (move-shape delta a-shape)
  (cond
    [(circle? a-shape) (move-circle delta a-shape)]
    [(rectangle? a-shape) (move-rectangle delta a-shape)]
    [(square? a-shape) (move-square delta a-shape)]))

(define (move-circle d s)
  (cond
    [(draw-and-clear-circle s) (translate-circle d s)]
    [else s]))

(define (move-rectangle d s)
  (cond
    [(draw-and-clear-rectangle s) (translate-rectangle d s)]
    [else s]))

(define (move-square d s)
  (cond
    [(draw-and-clear-square s) (translate-square d s)]
    [else s]))

;; (draw-shape 
;;  (move-shape 10
;;              (move-shape 10
;;                          (move-shape 10
;;                                      (move-shape 10 ci)))))

