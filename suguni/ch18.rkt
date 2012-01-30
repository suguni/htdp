;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch18) (read-case-sensitive #t) (teachpacks ((lib "hangman.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hangman.ss" "teachpack" "htdp")))))
;; ch 18.

;; ex 18.1.1
;; book

;; ex 18.1.2

;; 1.
;(local ((define x 10)
;        (y (+ x x)))
;  y)
;; [ERROR] local: expected a definition, but found a part

;; 2.
;(local ((define (f x) (+ (* x x) (* 3 x) 15))
;        (define x 100)
;        (define f@100 (f x)))
;  f@100 x)
;; [ERROR] local: expected only one expression after the local definitions, but found 1 extra part

;; 3.
;(local ((define (f x) (+ (* x x) (* 3 x) 14))
;        (define x 100)
;        (define f (f x)))
;  f)
;; [ERROR] local: f was defined locally more than once

;; ex 18.1.3
;; 1. X - a-nat-num
;; 2. O
;; 3. X - (define f x ...)

