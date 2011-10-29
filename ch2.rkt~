;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch2) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.ss" "teachpack" "htdp")))))
;; ex 2.1.1
(sqr 10)       ;; square
(sin (/ pi 2)) ;; sin
(> 10 20)      ;; compare

;; ex 2.1.2
(sqrt 4)
(sqrt 2)
(sqrt -1)
(tan (/ pi 4)) ;; tan

;; ex 2.2.1
(define (Fahrenheit->Celsius temp)
  (* (- temp 32) 5/9))

(convert-gui Fahrenheit->Celsius)
(convert-repl Fahrenheit->Celsius)
(convert-file "ex2.2.1-input.dat" Fahrenheit->Celsius "ex2.2.1-output.dat")
