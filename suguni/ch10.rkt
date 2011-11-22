;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch10) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")))))
;; ch.10

;; wage : number -> number
(define (wage h)
  (* h 12))

;; hours->wages : list of numbers -> list-of-numbers
(define (hours->wages lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (wage (first lon))
            (hours->wages (rest lon)))]))

;; (hours->wages empty) ;; empty
;; (hours->wages (cons 12 (cons 2 empty))) ;; (144 (24 empty))

;; ex 10.1.1
;; wage 함수를 변경하면 된다.
(define (wage-2 h)
  (* h 14))

;; ex 10.1.2
(define (hours->wages-2 lon)
  (cond
    [(empty? lon) empty]
    [else
     (cond
       [(> (first lon) 100)
        (error 'hours->wages "too many hours")]
       [else
        (cons (wage (first lon))
            (hours->wages-2 (rest lon)))])]))

;; test
;; (hours->wages-2 (cons 10 (cons 20 (cons 150 (cons 80 empty))))) ;; error

;; ex 10.1.3
;; Fahrenheit->Celsius : number -> number
(define (Fahrenheit->Celsius temp)
  (* (- temp 32) 5/9))

;; convertFC : list-of-numbers -> list-of-numbers
(define (convertFC lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (Fahrenheit->Celsius (first lon))
           (convertFC (rest lon)))]))

;; test
;; (convertFC empty) ;; empty
;; (convertFC (cons 32 empty)) ;; (cons 0 empty)
;; (convertFC (cons 32 (cons 41 empty))) ;; (cons 0 (cons 5 emtpy))

;; ex 10.1.4
;; convert-euro : list-of-numbers -> list-of-numbers
(define (convert-euro lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (* 1.22 (first lon))
           (convert-euro (rest lon)))]))
;; test
;; (convert-euro (cons 1 (cons 2 empty))) ;; (cons 1.22 (cons 2.44 empty))

;; convert-euro-1 : number, list-of-numbers -> list-of-numbers
(define (convert-euro-1 ratio lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (* ratio (first lon))
           (convert-euro-1 ratio (rest lon)))]))
;; test
;; (convert-euro-1 1.5 (cons 1 (cons 2 empty))) ;; (cons 1.5 (cons 3 empty))

;; ex 10.1.5
;; eliminate-exp : number, list-of-numbers -> list-of-numbers
(define (eliminate-exp ua lotp)
  (cond
    [(empty? lotp) empty]
    [else
     (cond
       [(>= ua (first lotp))
        (cons (first lotp) (eliminate-exp ua (rest lotp)))]
       [else
        (eliminate-exp ua (rest lotp))])]))

;; test
;; (eliminate-exp 1.0 (cons 2.95 (cons .95 (cons 1.0 (cons 5 empty)))))
;; => (cons .95 (cons 1.0 empty))

;; ex 10.1.6
;; name-robot : list-of-symbols -> list-of-symbols
