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
(define (name-robot los)
  (cond
    [(empty? los) empty]
    [else
     (cons
      (cond
        [(symbol=? (first los) 'robot) 'r2d2]
        [else (first los)])
      (name-robot (rest los)))]))
;; test
;(name-robot empty) ;; empty
;(name-robot (cons 'kitty empty)) ;; (cons 'kitty empty)
;(name-robot (cons 'kitty (cons 'robot (cons 'mazinga (cons 'robot empty)))))
;; => (cons 'kitty (cons 'r2d2 (cons 'mazinga (cons 'r2d2 empty))))

;; substitute : symbol symbol list-of-symbols -> list-of-symbols
(define (substitute new old los)
  (cond
    [(empty? los) empty]
    [else
     (cons
      (cond
        [(symbol=? (first los) old) new]
        [else (first los)])
      (substitute new old (rest los)))]))

;; test
;(substitute 'r2d2 'robot empty) ;; empty
;(substitute 'magic 'kitty (cons 'kitty empty)) ;; (cons 'magic empty)
;(substitute 'r2d2 'mazinga (cons 'kitty (cons 'robot (cons 'mazinga (cons 'robot empty)))))
;; => (cons 'kitty (cons 'robot (cons 'r2d2 (cons 'robot empty))))

;; ex 10.1.7
;; recall : symbol list-of-symbols -> list-of-symbols
(define (recall ty lon)
  (cond
    [(empty? lon) empty]
    [else
     (cond 
       [(symbol=? (first lon) ty) (recall ty (rest lon))]
       [else (cons (first lon) (recall ty (rest lon)))])]))

;; test
;; (recall 'robot (cons 'robot (cons 'doll (cons 'dress empty))))
;; => (cons 'doll (cons 'dress empty))

;; ex 10.1.8

;; Test set
;; (how-many 0 2 1)  => -1
;; (how-many 1 0 -1) => 2
;; (how-many 2 4 2)  => 1
;; (how-many 1 0 1)  => 0

(define (how-many a b c)
  (cond
    [(= a 0) -1]
    [(> (* b b) (* 4 a c)) 2]
    [(= (* b b) (* 4 a c)) 1]
    [(< (* b b) (* 4 a c)) 0]))

;; quadratic-roots : number number number -> symbol or number of list-of-numbers
(define (quadratic-roots a b c)
  (cond
    [(= (how-many a b c) -1) 'degenerate]
    [(= (how-many a b c)  0) 'none]
    [(= (how-many a b c)  1) (/ (- b) (* 2 a))]
    [(= (how-many a b c)  2)
     (cons
      (/ (+ (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a))
      (cons
       (/ (- (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a))
       empty))]))

;; test
;(quadratic-roots 0 2 1)  ;; 'degenerate
;(quadratic-roots 1 0 -1) ;; (cons 1 (cons -1 empty))
;(quadratic-roots 2 4 2)  ;; -1
;(quadratic-roots 1 0 1)  ;; 'none

;; ex 10.1.9

;; examples:
;; (controller 103)
;; results:
;; (cons 1 (cons 'dollar (cons 'and (cons 3 (cons 'cents empty)))))

;; contoller: number -> list
(define (controller money)
  (cons (quotient money 100)
        (cons (cond
                [(= (quotient money 100) 1) 'dollar]
                [else 'dollars])
              (cons 'and
                    (cons (remainder money 100)
                          (cons (cond
                                  [(= (remainder money 100) 1) 'cent]
                                  [else 'cents])
                                empty))))))

;; 0 dollars?
;; and 0 cents(or cent)?

