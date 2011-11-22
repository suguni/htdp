;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch5) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "master.ss" "teachpack" "htdp")))))
;; ex 5.1.1
(define (reply s)
  (cond
    [(symbol=? s 'GoodMorning?) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternood) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))
(reply 'HowAreYou?)

;; ex 5.1.2
;; check-guess : number number -> symbol
(define (check-guess guess target)
  (cond
    [(< guess target) 'TooSmall]
    [(= guess target) 'Perfect]
    [(> guess target) 'TooLarge]))
;; (guess-with-gui check-guess)

;; ex 5.1.3
;; comp : number number number -> number
(define (comp a b c)
  (+ a
     (* b 10)
     (* c 100)))

;; check-guess : number number number number -> symbol
(define (check-guess3 a b c target)
  (cond
    [(< (comp a b c) target) 'TooSmall]
    [(= (comp a b c) target) 'Perfect]
    [(> (comp a b c) target) 'TooLarge]))

;; ex 5.1.4
;; what-kind : number number number -> symbol
;(define (what-kind a b c)
;  (cond
;    [(= a 0)               'degenerate]
;    [(> (* b b) (* 4 a c)) 'two]
;    [(= (* b b) (* 4 a c)) 'one]
;    [(< (* b b) (* 4 a c)) 'none]))
;; => [TODO] use 4.4.4 how-many!!!

(define (how-many a b c)
  (cond
    [(= a 0) -1]
    [(> (* b b) (* 4 a c)) 2]
    [(= (* b b) (* 4 a c)) 1]
    [(< (* b b) (* 4 a c)) 0]))

(define (what-kind a b c)
  (cond
    [(= (how-many a b c) -1) 'degenerate]
    [(= (how-many a b c) 2)  'two]
    [(= (how-many a b c) 1)  'one]
    [(= (how-many a b c) 0)  'none]))


'what-kind
(symbol=? (what-kind 1 0 -1) 'two)
(symbol=? (what-kind 2 4 2)  'one)
(symbol=? (what-kind 0 2 1)  'degenerate)
(symbol=? (what-kind 1 1 1)  'none)

;; ex 5.1.5
;; check-color : symbol symbol symbol symbol -> symbol
(define (check-color target1 target2 guess1 guess2)
  (cond
    [(and (symbol=? guess1 target1) (symbol=? guess2 target2)) 'Perfect]
    [(or (symbol=? guess1 target1) (symbol=? guess2 target2))  'OneColorAtCorrectPosition]
    [(or (symbol=? guess1 target2) (symbol=? guess2 target1))  'OneColorOccurs]
    [else                                                      'NothingCorrect]))
