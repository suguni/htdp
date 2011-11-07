;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6-traffic-light) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")))))
;; ch 6. page 70
;; traffic light

;; dimensions of traffic light 
(define WIDTH 200)
(define HEIGHT 160)
(define BULB-RADIUS 20)
(define BULB-DISTANCE 10)

;; the positions of the bulbs 
(define X-BULBS (quotient WIDTH 2))
(define Y-RED (+ BULB-DISTANCE BULB-RADIUS))
(define Y-YELLOW (+ Y-RED BULB-DISTANCE (* 2 BULB-RADIUS)))
(define Y-GREEN (+ Y-YELLOW BULB-DISTANCE (* 2 BULB-RADIUS)))

;; draw the light with the red bulb turned on
(start WIDTH HEIGHT)
;(draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
;(draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
;(draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)

;; ex 6.2.2
;; clear-bulb : color->boolean
(define (clear-bulb color)
  (cond
    [(symbol=? color 'red)
     (and
      (clear-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS)
      (draw-circle (make-posn X-BULBS Y-RED) BULB-RADIUS color))]
    [(symbol=? color 'yellow)
     (and
      (clear-solid-disk (make-posn X-BULBS Y-YELLOW) BULB-RADIUS)
      (draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS color))]
    [(symbol=? color 'green)
     (and
      (clear-solid-disk (make-posn X-BULBS Y-GREEN) BULB-RADIUS)
      (draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS color))]
    [else false]))

;; ex 6.2.3
;; draw-bulb : color->boolean
(define (draw-bulb color)
  (cond
    [(symbol=? color 'red)
     (and
      (clear-circle (make-posn X-BULBS Y-RED) BULB-RADIUS)
      (draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS color))]
    [(symbol=? color 'yellow)
     (and
      (clear-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS)
      (draw-solid-disk (make-posn X-BULBS Y-YELLOW) BULB-RADIUS color))]
    [(symbol=? color 'green)
     (and
      (clear-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS)
      (draw-solid-disk (make-posn X-BULBS Y-GREEN) BULB-RADIUS color))]
    [else false]))

;; ex 6.2.4
;; switch : color, color -> boolean
(define (switch color1 color2)
  (and
   (clear-bulb color1)
   (draw-bulb color2)))

;; 없는 색을 넣을 경우 false를 반환하고, 정상적으로 완료가 되지 않는다.
;; (switch 'red 'blue) 라고 하면, red는 꺼지고 false 반환된다.
;; 수정해야 하나?

;; ex 6.2.5
(define (next current-color)
  (cond
    [(and (symbol=? current-color 'red) (switch 'red 'green)) 'green]
    [(and (symbol=? current-color 'yellow) (switch 'yellow 'red)) 'red]
    [(and (symbol=? current-color 'green) (switch 'green 'yellow)) 'yellow]))

;; 뭘 하라는 거지???
(draw-bulb 'red)
(draw-bulb 'red)
(draw-bulb 'red)
(next (next (next (next 'red))))

