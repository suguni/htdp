;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter.9) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;ex.9.1.2
(define l (cons 10 (cons 20 (cons 6 empty))))

;ex.9.3.1
(define (contain-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else
     (cond [(symbol=? (first a-list-of-symbols) 'doll) true]
           [else (contain-doll? (rest a-list-of-symbols))])]))

(contain-doll? empty)

;ex.9.3.2
(define (contain-doll-2? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else
     (cond [(symbol=? (first a-list-of-symbols) 'doll) true]
           [else (contain-doll-2? (rest a-list-of-symbols))])]))

;ex.9.3.3
(define (contain? a-list-of-symbols a-symbol)
  (cond
    [(empty? a-list-of-symbols) false]
    [else
     (cond [(symbol=? (first a-list-of-symbols) a-symbol) true]
           [else (contain-doll? (rest a-list-of-symbols))])]))

;ex.9.5.1
;(define (sum a-list