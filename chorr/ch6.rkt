;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
(make-posn 3 4)
(make-posn 8 6)
(make-posn 5 12)

(define a-posn (make-posn 7 0))
(posn-x a-posn)
(posn-y a-posn)

(define (distance-to-0 a-posn)
  (sqrt
    (+ (sqr (posn-x a-posn))
       (sqr (posn-y a-posn)))))

;; ex 6.1.1
(distance-to-0 (make-posn 3 4))
(distance-to-0 (make-posn (* 2 3) (* 2 4)))
(distance-to-0 (make-posn 12 (- 6 1)))

;; ex 6.2.1
;(start 300 300)
;(draw-solid-line (make-posn 10 10) (make-posn 110 10) 'red)
;(draw-solid-rect (make-posn 10 30) 100 50 'blue)
;(draw-circle (make-posn 110 30) 30 'yellow)
;(draw-solid-disk (make-posn 10 80) 50 'green)
;(stop)


;; dimensions of traffic light    
(define WIDTH 50)
(define HEIGHT 160)
(define BULB-RADIUS 20)
(define BULB-DISTANCE 10)

;; the positions of the bulbs 
(define X-BULBS (quotient WIDTH 2))
(define Y-RED (+ BULB-DISTANCE BULB-RADIUS))
(define Y-YELLOW (+ Y-RED BULB-DISTANCE (* 2 BULB-RADIUS)))
(define Y-GREEN (+ Y-YELLOW BULB-DISTANCE (* 2 BULB-RADIUS)))

;; draw the light with the red bulb turned on
;(start WIDTH HEIGHT)
;(draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
;(draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
;(draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)

;; ex 6.2.2
; clear-bulb : color -> boolean
(define (clear-bulb color)
  (cond
    [(symbol=? color 'red) (and (clear-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
                                (draw-circle (make-posn X-BULBS Y-RED) BULB-RADIUS 'red))]
    [(symbol=? color 'yellow) (and (clear-solid-disk (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
                                (draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow))]
    [(symbol=? color 'green) (and (clear-solid-disk (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)
                                (draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green))]
    [else false]))

;; ex 6.2.3
; draw-bulb : color -> boolean
(define (draw-bulb color)
  (cond
    [(symbol=? color 'red) (and (clear-circle (make-posn X-BULBS Y-RED) BULB-RADIUS)
                                (draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red))]
    [(symbol=? color 'yellow) (and (clear-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS)
                                   (draw-solid-disk (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow))]
    [(symbol=? color 'green) (and (clear-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS)
                                  (draw-solid-disk (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green))]
    [else false]))

;; ex 6.2.4
; switch : color color -> boolean
(define (switch c1 c2)
  (and
   (clear-bulb c1)
   (draw-bulb c2)))

;; ex 6.2.5
;; next : symbol  ->  symbol
;; to switch a traffic light's current color and to return the next one 
(define (next current-color)
  (cond
    [(and (symbol=? current-color 'red) (switch 'red 'green))
     'green]
    [(and (symbol=? current-color 'yellow) (switch 'yellow 'red))
     'red]
    [(and (symbol=? current-color 'green) (switch 'green 'yellow))
     'yellow]))

;(start WIDTH HEIGHT)
;(draw-bulb 'red)
;(next (next (next (next 'red))))

;; ex 6.3.1
(define-struct boyfriend (name hair eyes phone))
(define-struct cheerleader (name number))
(define-struct CD (artist title price))
(define-struct sweater (material size producer))
;단순한 구조여서 딱히 설명 생략

;; ex 6.3.2
(define-struct movie (title producer))
(movie-title (make-movie 'ThePhantomMenace 'Lucas))
(movie-producer (make-movie 'TheEmpireStrikesBack 'Lucas))

;; ex 6.3.3
(define-struct fighter (name accel max-speed range))
(define (within-range info dist)
  (>= (fighter-range info) dist))
(define (reduce-range info)
  (make-fighter (fighter-name info)
                (fighter-accel info)
                (fighter-max-speed info)
                (* (fighter-range info) 0.8)))

(define f22 (make-fighter 'f22 50 200 1000))

;; ex 6.4.1
(define-struct movie (title producer))
;여기에서 title, producer는 기호다.
(define-struct boyfriend (name hair eyes phone))
;여기에서 name, hair, eyes는 기호이고, phone은 수다.
(define-struct cheerleader (name number))
;여기에서 name은 기호이고, number는 수다.
(define-struct CD (artist title price))
;여기에서 artist, title은 기호이고, price는 수다.
(define-struct sweater (material size producer))
;여기에서 material, producer은 기호이고, size는 수다.

;; ex 6.4.2
;over-time은 구조체이다
(make-over-time hour minute second)
;여기에서 hour, minute, second는 수다.

;; ex 6.4.3
;3-length-word은 구조체이다.
(make-3-length-word c1 c2 c3)
;여기에서 c1, c2, c3는 기호이다.

