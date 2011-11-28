;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "hangman.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "hangman.ss" "teachpack" "htdp")))))
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
;(movie-title (make-movie x y)) = x
;(movie-producer (make-movie x y)) = y

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
#|
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
|#

;; ex 6.4.2
;over-time은 구조체이다
(define-struct time (hour minute second))
;여기에서 hour, minute, second는 수다.

;; ex 6.4.3
;3-length-word은 구조체이다.
(define-struct 3-length-word (c1 c2 c3))
;여기에서 c1, c2, c3는 기호이다.

;; ex 6.5.1
#|
(define-struct movie (title producer))
;; process-movie : movie -> ???
(define (process-movie a-movie)
  ... (movie-title a-movie) ...
  ... (movie-producer a-movie) ...)
|#
; 위와 동일한 방식

;; ex 6.5.2
;; time->seconds : over-time -> number
(define (time->seconds time)
  (+ (* (time-hour time) 60 60)
     (* (time-minute time) 60)
     (time-second time)))

(check-expect (time->seconds (make-time 12 30 2)) 45002)

;; ex 6.6.1
(define-struct circle (center radius stroke-color))

;; 템플릿
;; fun-for-circle : circle -> ???
#|
(define (fun-for-circle a-circle)
  ... (circle-center a-circle) ...
  ... (circle-radius a-circle) ...
  ... (circle-stroke-color a-circle) ...)
|#

;; ex 6.6.2
(define (draw-a-circle c)
  (draw-circle (circle-center c)
               (circle-radius c)
               (circle-stroke-color c)))

;; ex 6.6.3
;; in-circle? : circle posn -> boolean
(define (in-circle? c p)
  (> (circle-radius c)
     (distance-to-0 
      (make-posn (- (posn-x (circle-center c))
                    (posn-x p))
                 (- (posn-y (circle-center c))
                    (posn-y p))))))
;; 테스트:
(define circle-1 (make-circle (make-posn 6 2) 1 'red))
(check-expect (in-circle? circle-1 (make-posn 6 1.5)) true)
(check-expect (in-circle? circle-1 (make-posn 8 6)) false)

;; ex 6.6.4
;; translate-circle : circle number -> circle
(define (translate-circle c delta)
  (make-circle 
   (make-posn (+ (posn-x (circle-center c)) delta)
              (posn-y (circle-center c)))
   (circle-radius c)
   (circle-stroke-color c)))
;; 테스트:
(define circle-2 (make-circle (make-posn 6 2) 1 'red))
(check-expect (translate-circle circle-2 3)
              (make-circle (make-posn 9 2) 1 'red))

;; ex 6.6.5
;; clear-a-circle : circle -> boolean
(define (clear-a-circle c)
  (clear-circle (circle-center c)
                (circle-radius c)
                (circle-stroke-color c)))

;; ex 6.6.6
;; draw-and-clear-circle : circle -> boolean
(define (draw-and-clear-circle c)
  (and (draw-a-circle c)
       (sleep-for-a-while 0.5)
       (clear-a-circle c)))

;; move-circle : number circle  ->  circle
(define (move-circle delta a-circle)
  (cond
    [(draw-and-clear-circle a-circle) (translate-circle a-circle delta)]
    [else a-circle]))

#| 실행 예
(start 200 100)
(draw-a-circle 
  (move-circle 10
    (move-circle 10
      (move-circle 10
	(move-circle 10 circle-1)))))
|#

;; ex 6.6.7
;; ex 6.6.8
;; ex 6.6.9
;; ex 6.6.10
;; ex 6.6.11
;; ex 6.6.12
;; 이상 위의 circle 구조체와 동일한 형태


;; ex 6.7.1

;; http://pre.plt-scheme.org/racket/collects/htdp/tests/draw.rkt
;; draw-next-part : symbol -> true
;; consumes one of the seven body-part symbols and draws that part.

(define (draw-next-part body-part)
  (cond 
    [(eq? body-part 'body)
     (draw-solid-line (make-posn 100 60) (make-posn 100 130) 'black)]
    [(eq? body-part 'right-leg)
     (draw-solid-line (make-posn 100 130) (make-posn 30 170) 'black)]
    [(eq? body-part 'left-leg)
     (draw-solid-line (make-posn 100 130) (make-posn 170 170) 'black)]
    [(eq? body-part 'right-arm)
     (draw-solid-line (make-posn 100 75) (make-posn 40 65) 'black)]
    [(eq? body-part 'left-arm)
     (draw-solid-line (make-posn 100 75) (make-posn 160 65) 'black)]
    [(eq? body-part 'head)
     (draw-solid-disk (make-posn 100 50) 10 'black)]
    [(eq? body-part 'noose)
     (and
      (draw-solid-disk (make-posn 120 50) 30 'red)
      (draw-solid-line (make-posn 100 30) (make-posn 100 10) 'black)
      (draw-solid-line (make-posn 100 10) (make-posn 0 10) 'black)
      (draw-solid-line (make-posn 115 35) (make-posn 123 43) 'black)
      (draw-solid-line (make-posn 123 35) (make-posn 115 43) 'black)
      (draw-solid-line (make-posn 131 40) (make-posn 139 48) 'black)
      (draw-solid-line (make-posn 139 40) (make-posn 131 48) 'black))]))

;; ex 6.7.2
(define-struct word (c1 c2 c3))

;; ex 6.7.3
;; exact-character : symbol symbol symbol -> symbol
(define (exact-character answer current guess)
  (cond
    [(or (eq? answer current)
         (eq? answer guess)) answer]
    [else '_]))
; 테스트:
(check-expect (exact-character 't '_ 'u) '_)
(check-expect (exact-character 'e 'e 'u) 'e)
(check-expect (exact-character 'l '_ 'l) 'l)

;; reveal : word word symbol -> word
(define (reveal answer current guess)
  (make-word 
   (exact-character (word-c1 answer)
                    (word-c1 current)
                    guess)
   (exact-character (word-c2 answer)
                    (word-c2 current)
                    guess)
   (exact-character (word-c3 answer)
                    (word-c3 current)
                    guess)))
; 테스트:
(check-expect (reveal (make-word 't 'e 'a) (make-word '_ 'e '_) 'u)
              (make-word '_ 'e '_))
(check-expect (reveal (make-word 'a 'l 'e) (make-word 'a '_  '_) 'e)
              (make-word 'a '_ 'e))
(check-expect (reveal (make-word 'a 'l 'l) (make-word '_ '_ '_) 'l)
              (make-word '_ 'l 'l))

; 실행:
; (start 200 200)
; (hangman make-word reveal draw-next-part)