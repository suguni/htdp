;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")))))
;; ch 6
;; ch 6.1

;(make-posn 3 4)
;(posn-x (make-posn 3 4))
;(posn-y (make-posn 3 4))

;; distance-to-0 : posn->number
;; 원점에서 a-posn까지의 거리를 계산한다.
(define (distance-to-0 a-posn)
 (sqrt
  (+ (sqr (posn-x a-posn))
     (sqr (posn-y a-posn)))))

;; ex 6.1.1
'make-posn-3-4
(distance-to-0 (make-posn 3 4))
;(sqrt
; (+ (sqr (posn-x (make-posn 3 4)))
;    (sqr (posn-y (make-posn 3 4)))))
;(sqrt
; (+ (sqr 3)
;    (sqr 4)))
;(sqrt (+ 9 16))
;(sqrt 25)
;5

'make-posn-6-8
(distance-to-0 (make-posn (* 2 3) (* 2 4)))
;(distance-to-0 (make-posn 6 8))
;(sqrt
; (+ (sqr (posn-x (make-posn 6 8)))
;    (sqr (posn-y (make-posn 6 8)))))
;(sqrt
; (+ (sqr 6)
;    (sqr 8)))
;(sqrt (+ 36 64))
;(sqrt 100)
;10

'make-posn-12-5
(distance-to-0 (make-posn 12 (- 6 1)))
;(distance-to-0 (make-posn 12 5))
;(sqrt
; (+ (sqr (posn-x (make-posn 12 5)))
;    (sqr (posn-y (make-posn 12 5)))))
;(sqrt
; (+ (sqr 12)
;    (sqr 5)))
;(sqrt (+ 144 25))
;(sqrt 169)
;13

;; ch 6.2
;; ex 6.2.1
(start 300 300)
(draw-solid-line (make-posn 1 1) (make-posn 5 5) 'red)
(draw-solid-rect (make-posn 20 10) 50 200 'blue)
(draw-circle (make-posn 200 10) 50 'red)
(draw-solid-disk (make-posn 200 10) 50 'green)
(stop)

;; ch6-traffic-light.rkt
;; ex 6.2.2 ~ 6.2.5

;; ch 6.3

;; ex 6.3.1
(define-struct movie (title producer))
;; movie : movie-title, movie-producer
(define-struct boyfriend (name hair eyes phone))
;; boyfriend : boyfriend-name, boyfriend-hair, boyfriend-eyes, boyfriend-phone
(define-struct cheerleader (name number))
;; cheerleader : cheerleader-name, cheerleader-number
(define-struct CD (artist title price))
;; CD : CD-artist, CD-title, CD-price
(define-struct sweater (material size producer))
;; sweater : sweater-material, sweater-size, sweater-producer

;; ex 6.3.2
'movie-struct
(movie-title (make-movie 'ThePhantomMenace 'Lucas))
(movie-producer (make-movie 'TheEmpireStrikesBack 'Lucas))

;; (movie-title (make-movie x y)) => x
;; (movie-producer (make-movie x y)) => y
;; 법칙은 모르겠다.

;; ex 6.3.3
(define-struct fighter (name acceleration max-speed range))

;; within-range : fighter, number -> boolean
(define (within-range fighter target-distance)
  (> (fighter-range fighter) target-distance))

;; reduce-range : fighter -> fighter
(define (reduce-range fighter)
  (make-fighter
   (fighter-name fighter)
   (fighter-acceleration fighter)
   (fighter-max-speed fighter)
   (* (fighter-range fighter) 0.8)))

;; ch 6.4

;; ex 6.4.1
;; 앞에서 정의되어 있어서 define-struck는 주석처리

;; A movie is a structure:
;(define-struct movie (title producer))
;; where title and producer are symbols.

;; A boyfriend is a structure:
;(define-struct boyfriend (name hair eyes phone))
;; where name, hair, eyes and phone are symbols.

;; A cheerleader is a structure:
;(define-struct cheerleader (name number))
;; where name is a symbol and number is a number.

;; A CD is a structure:
;(define-struct CD (artist title price))
;; where title is a symbols and price is a number.

;; A seater is a structure:
;(define-struct sweater (material size producer))
;; where material and producer are symbols and size is a number.

;; ex 6.4.2
;; A time-since-midnight is a structure:
(define-struct time-since-midnight (hours minutes seconds))
;; where hours, minutes and secods are number.

;; ex 6.4.3
;; A three-letters-word is a structure:
(define-struct three-letters-word (first-letter second-letter third-letter))
;; where first-letter, second-letter and third-letter are symbols.

;; ch 6.5
;; 복합데이터를 다룰때 디자인 레시피(디자인 프로세스)

;; ex 6.5.1
;; struct 정의는 ex 6.3.1에서 했음

;; movie
;; (define (process-movie a-movie)
;;   ... (movie-title a-movie) ...
;;   ... (movie-producer a-movie) ... )

;; boyfriend
;; (define (process-boyfriend a-boyfriend)
;;   ... (boyfriend-name a-boyfriend) ...
;;   ... (boyfriend-hair a-boyfriend) ...
;;   ... (boyfriend-eyes a-boyfriend) ...
;;   ... (boyfriend-phone a-boyfriend) ...)

;; cheerleader
;; (define (process-cheerleader a-cheerleader)
;;   ... (cheerleader-name a-cheerleader) ...
;;   ... (cheerleader-number a-cheerleader) ...)

;; CD
;; (define (process-CD a-CD)
;;   ... (CD-artist a-CD) ...
;;   ... (CD-title a-CD) ...
;;   ... (CD-price a-CD) ...)

;; sweater
;; (define (process-sweater a-sweater)
;;   ... (sweater-material a-sweater) ...
;;   ... (sweater-size a-sweater) ...
;;   ... (sweater-producer a-sweater) ...)

;; ex 6.5.2

;; Data Analysis & Definitions:
(define-struct time (hours minutes seconds))
;; A time is a structure: (make-time h m s) where h, m, and s are numbers.

;; Template:
;; (define (process-time a-time)
;;   ... (time-hours a-time) ...
;;   ... (time-minutes a-time) ...
;;   ... (time-seconds a-time) ...)

;; Purpose: time 구조체가 나타내는 시간까지 경과한 초를 계산한다.
;; Contract: time-seconds : time -> number

;; Examples:
;; (time->seconds (make-time 12 30 2))
;; =
;; 45002

;; Definition:
(define (time->seconds t)
  (+ (* (time-hours t) 60 60)
     (* (time-minutes t) 60)
     (time-seconds t)))

;; Tests:
(time->seconds (make-time 12 30 2))
;; expected value:
45002

