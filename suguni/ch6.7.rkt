;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6.7) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "hangman.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "hangman.ss" "teachpack" "htdp")))))
;; ch 6.7

;; ex 6.7.1

;; Contract:
;; draw-next-part : symbol -> true
;; symbol : 'right-leg 'left-leg 'left-arm 'right-arm 'body 'head 'noose

;; Purpose:
;; Draws the pieces of a hangman figure.

;; Examples:
;; (draw-next-part 'right-leg)

;; Definition:
(define (draw-next-part part)
  (cond
    [(symbol=? part 'head)
     (draw-circle (make-posn 100 50) 10 'black)]
    [(symbol=? part 'body)
     (draw-solid-line (make-posn 100 60) (make-posn 100 130) 'black)]
    [(symbol=? part 'right-arm)
     (draw-solid-line (make-posn 100 75) (make-posn 160 65) 'black)]
    [(symbol=? part 'left-arm)
     (draw-solid-line (make-posn 100 75) (make-posn 40 65) 'black)]
    [(symbol=? part 'right-leg)
     (draw-solid-line (make-posn 100 130) (make-posn 165 160) 'black)]
    [(symbol=? part 'left-leg)
     (draw-solid-line (make-posn 100 130) (make-posn 35 160) 'black)]
    [(symbol=? part 'noose)
     (and
      (draw-solid-line (make-posn 0 10) (make-posn 100 10) 'black)
      (draw-solid-line (make-posn 100 10) (make-posn 100 30) 'black)
      (draw-circle (make-posn 120 50) 30 'red)
      (draw-solid-line (make-posn 115 35) (make-posn 125 45))
      (draw-solid-line (make-posn 125 35) (make-posn 115 45))
      (draw-solid-line (make-posn 130 40) (make-posn 140 50))
      (draw-solid-line (make-posn 140 40) (make-posn 130 50)))]
    [else false]))

;; Tests:
;; (start 200 300)
;; (draw-next-part 'head)
;; (draw-next-part 'body)
;; (draw-next-part 'left-arm)
;; (draw-next-part 'right-arm)
;; (draw-next-part 'left-leg)
;; (draw-next-part 'right-leg)
;; (draw-next-part 'noose)

;; ex 6.7.2

;; Data Analysis & Definitions:
(define-struct word (first second third))
;; A word is a structure:
;; (make-word f s t) where f, s and t are symbols.

;; Template:
;; (define (fun-for-word a-word)
;;   ... (word-first a-word) ...
;;   ... (word-second a-word) ...
;;   ... (word-third a-word) ...)

;; ex 6.7.3

;; Contract:
;; reveal : word word symbol -> word

;; Purpose:
;; The function produces a new status word, that is, a word that contains
;; ordinary letters and '_. The fields in the new status word are determined by
;; comparing the guess with each pair of letters from the status word and the
;; chosen word:
;;  1. If the guess is equal to the letter in the chosen word,
;;     the guess is the corresponding letter in the new status word.
;;  2. Otherwise, the new letter is the corresponding letter from the status word.

;; Auxiliary Functions:

;; Contract:
;; reveal : symbol symbol symbol -> symbol
(define (reveal-one chosen status guess)
  (cond
    [(symbol=? status '_)
     (cond
       [(symbol=? chosen guess) guess]
       [else '_])]
    [else status]))

;; Definition:
(define (reveal chosen status guess)
  (make-word
   (reveal-one (word-first chosen) (word-first status) guess)
   (reveal-one (word-second chosen) (word-second status) guess)
   (reveal-one (word-third chosen) (word-third status) guess)))

;; Tests:
(reveal (make-word 't 'e 'a) (make-word '_ 'e '_) 'u)
;; expected value
(make-word '_ 'e '_)

(reveal (make-word 'a 'l 'e) (make-word 'a '_  '_) 'e)
;; expected value: 
(make-word 'a '_ 'e)

(reveal (make-word 'a 'l 'l) (make-word '_ '_ '_) 'l)
;; expected value
(make-word '_ 'l 'l)

;; game
;; (start 200 300)
;; (hangman make-word reveal draw-next-part)
