;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch5) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp")))))
(define (reply s)
  (cond
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))

;; ex 5.1.1
(reply 'HowAreYou?)
(symbol=? 'Hello 'Hello)
(symbol=? 'Hello 'World)

;; ex 5.1.2
(define (check-guess guess target)
  (cond
    [(= guess target) 'Perfect]
    [(> guess target) 'TooLarge]
    [else 'TooSmall]))
;(guess-with-gui check-guess)

;; ex 5.1.3
(define (check-guess3 g1 g2 g3 target)
  (cond
    [(= (reverse-num g1 g2 g3) target) 'Perfect]
    [(> (reverse-num g1 g2 g3) target) 'TooLarge]
    [else 'TooSmall]))
(define (reverse-num g1 g2 g3)
  (+ (* g3 100) (* g2 10) g1))
;(check-guess3 1 2 3 321)
;-> Perfect
;(check-guess3 4 6 8 264)
;-> TooLarge
;(check-guess3 4 6 8 987)
;-> TooSmall
;(guess-with-gui-3 check-guess3)

;; ex 5.1.4
(define (what-kind a b c)
  (cond
    [(= a 0) 'degenerate]
    [(> (sqr b) (* 4 a c)) 'two]
    [(= (sqr b) (* 4 a c)) 'one]
    [else 'none]))
;(what-kind 1 2 3)
;-> none
;(what-kind 1 2 1)
;-> one
;(what-kind 4 1 -8)
;-> two

;; ex 5.1.5
(define (check-color g1 g2 t1 t2)
  (cond
    [(and (symbol=? g1 t1) (symbol=? g2 t2)) 'Perfect]
    [(or (symbol=? g1 t1) (symbol=? g2 t2)) 'OneColorAtCorrentPosition]
    [(or (symbol=? g1 t2) (symbol=? g2 t1)) 'OneColorOccurs]
    [else 'NothingCorrect]))
;(master check-color)
