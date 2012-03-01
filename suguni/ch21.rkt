;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch21) (read-case-sensitive #t) (teachpacks ((lib "dir.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.ss" "teachpack" "htdp")))))
;; ch 21

;; ex 21.1.1
;; tabulate : (x -> y) (list x) -> (list y)
(define (tabulate f n)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons (f n)
           (tabulate f (sub1 n)))]))

;; tabulate-sin : number -> lon
(define (tabulate-sin n)
  (tabulate sin n))

;; tabulate-sqrt : number -> lon
(define (tabulate-sqrt n)
  (tabulate sqrt n))

;; tabulate-sqr : number -> lon
(define (tabulate-sqr n)
  (tabulate sqr n))

;; tabulate-tan : number -> lon
(define (tabulate-tan n)
  (tabulate tan n))

;; ex 21.1.2
;; fold : (X -> Y) Y (listof X) -> Y
(define (fold f base lox)
  (cond
    [(empty? lox) base]
    [else (f (first lox)
             (fold f base (rest lox)))]))

(check-expect (fold + 0 (list 1 2 3 4 5 6 7 8 9 10)) 55)
(check-expect (fold * 1 (list 1 2 3 4 5)) 120)

;; my-append : (listof X) (listof X) -> (listof X) ???
(define (my-append lox loy)
  (fold cons loy lox))

(check-expect (my-append (list 1 2 3) (list 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))

;; my-map : (X -> Y) (listof X) -> (listof Y)
(define (my-map f seq)
  (cond
    [(empty? seq) empty]
    [else
     (fold cons 
           (my-map f (rest seq))
           (list (f (first seq))))]))

(check-expect (my-map add1 (list 1 2 3)) (list 2 3 4))

;; ex 21.1.3
;(define (n-adder n x)
;  (cond
;    [(zero? n) x]
;    [else (+ 1 (n-adder (sub1 n) x))]))
;(define (copy n obj)
;  (cond
;    [(zero? n) empty]
;    [else (cons obj (copy (sub1 n) obj))]))

;; natural-f : Number X -> (listof X)
(define (natural-f fn base n x)
  (cond
    [(zero? n) base]
    [else (fn x (natural-f fn base (sub1 n) x))]))

(check-expect (natural-f cons empty 3 'a) (list 'a 'a 'a))
(check-expect (natural-f + 5 3 1 ) 8)

(define (copy n obj) (natural-f cons empty n obj))
(define (n-adder n x) (natural-f + x n 1))

(check-expect (copy 3 'a) (list 'a 'a 'a))
(check-expect (n-adder 3 5) 8)

;; n-multiploer : number number -> number
(define (n-multiplier n x)
  (natural-f + 0 n x))

;; tests
(check-expect (n-multiplier 3 5) 15)

;; ex 21.2.1
;; 1.
(check-expect (build-list 4 identity)
              (list 0 1 2 3))
(check-expect (build-list 4 add1)
              (list 1 2 3 4))

;; 2.
(define (expt-n1 n) (/ 1 (expt 10 (add1 n))))
(check-expect (build-list 4 expt-n1)
              (list .1 .01 .001 .0001))

;; 3.
;; evens : number -> (listof number)
(define (evens n)
  (local ((define (double n)
            (* (+ n 1) 2)))
    (build-list n double)))

;; tests
(check-expect (evens 3) (list 2 4 6))
(check-expect (evens 6) (list 2 4 6 8 10 12))

;; 4.
;; tabulate-bl : (x -> y) (list x) -> (list y)
(define (tabulate-bl f n)
  (build-list n f))

;; tests
(check-expect (tabulate-bl add1 5) (list 1 2 3 4 5))

;; 5.
;; diagonal : number -> (listof (listof number))
(define (diagonal n)
  (local ((define (row c r)
            (cond
              [(= c n) empty]
              [else
               (cond
                 [(= c r) (cons 1 (row (add1 c) r))]
                 [else (cons 0 (row (add1 c) r))])]))
          (define (mat r)
            (row 0 r)))
    (build-list n mat)))

;; tests
(check-expect (diagonal 3)
              (list
               (list 1 0 0)
               (list 0 1 0)
               (list 0 0 1)))
(check-expect (diagonal 2)
              (list
               (list 1 0)
               (list 0 1)))

;; ex 21.2.2
;; 1.
(define (convert-euro lod)
  (local ((define (dollar->euro d)
            (* d 1.22)))
    (map dollar->euro lod)))
(check-expect (convert-euro (list 1 2 3 4)) (list 1.22 2.44 3.66 4.88))

;; 2.
(define (convertFC lof)
  (local ((define (Fahrenheit->Celsius temp)
            (* (- temp 32) 5/9)))
    (map Fahrenheit->Celsius lof)))

;; 3.
(define (move-all lop)
  (local ((define (move3 p) (make-posn (+ (posn-x p) 3) (posn-y p))))
    (map move3 lop)))

;; tests
(check-expect (move-all (list (make-posn 1 2) (make-posn 4 7)))
              (list (make-posn 4 2) (make-posn 7 7)))

;; ex 21.2.3
;; 1.
(define-struct toy (name exp))

(define (eliminate-exp ua lot)
  (local ((define (pred? t)
            (< (toy-exp t) ua)))
    (filter pred? lot)))

(check-expect (eliminate-exp 10 (list (make-toy 'a 20) (make-toy 'b 5) (make-toy 'c 2) (make-toy 'd 23)))
              (list (make-toy 'b 5) (make-toy 'c 2)))

;; 2.
;; recall : symbol (listof symbol) -> (listof symbol)
(define (recall ty lon)
  (local ((define (pred? t)
            (not (symbol=? t ty))))
    (filter pred? lon)))

;; test
(check-expect (recall 'a (list 'b 'c 'x 'a))
              (list 'b 'c 'x))

;; 3.
;; selection : (listof symbol) (listof symbol) -> (listof symbol)
(define (selection los1 los2)
  (local ((define (fn x)
            (local ((define (pred? y)
                      (symbol=? x y)))
              (filter pred? los2))))
    (fold append (list) (map fn los1))))

;; tests
(check-expect (selection (list 'a 'b 'c) (list 'b 'c 'd)) (list 'b 'c))
