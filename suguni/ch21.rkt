;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch21) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp") (lib "arrow.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp") (lib "arrow.ss" "teachpack" "htdp")))))
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

;; ex 21.4.1
;; Shape definitions from ex 7.4.1
;; Data Definitions:
(define-struct circle (center radius color))
;; A circle is a structure:
;;          (make-circle p s c)
;;    where p is a posn, s is a number and c is a color;

;; process-circle
;; abstract draw-circle/clear-circle
(define (process-circle fn c)
  (fn (circle-center c)
      (circle-radius c)
      (circle-color c)))

;; draw-a-circle : circle -> true
;; draw a circle s
(define (draw-a-circle c)
  (process-circle draw-circle c))

;; clear-a-circle : circle -> true
;; clear a circle s
(define (clear-a-circle c)
  (process-circle clear-circle c))

;; translate-circle : circle, number -> circle
;; 보조함수를 define으로 정의하라고 했다. d(이동거리) 인자가 필요하므로 local로 정의함.
;(define (translate-circle s d)
;  (local ((define (translate p r c)
;            (make-circle
;             (make-posn (+ d (posn-x p)) (posn-y p))
;             r c)))
;    (process-circle translate s)))

;; tests
;(check-expect (translate-circle (make-circle (make-posn 0 0) 10 'red) 10)
;              (make-circle (make-posn 10 0) 10 'red))

;; ex 21.4.2
(define-struct rectangle (nw width height color))
;; A rectangle is a structure:
;;          (make-rectangle p w h c)
;;    where p is a posn, w and h a number and c is a color.

;; process-rectagnle : [posn number number color -> X] rectagle -> X
(define (process-rectangle fn a-rect)
  (fn (rectangle-nw a-rect)
      (rectangle-width a-rect)
      (rectangle-height a-rect)
      (rectangle-color a-rect)))

;; draw-a-rectangle : rectangle -> true
;; draw a rectangle s
(define (draw-a-rectangle a-rect)
  (process-rectangle draw-solid-rect a-rect))

;; clear-a-rectangle : rectangle -> true
;; clear a rectangle s
(define (clear-a-rectangle a-rect)
  (process-rectangle clear-solid-rect a-rect))

;; translate-rectangle : rectangle, number -> rectangle
;(define (translate-rectangle a-rect delta)
;  (local ((define (translate a-posn width height color)
;            (make-rectangle
;             (make-posn (+ delta (posn-x a-posn))
;                        (posn-y a-posn))
;             width height color)))
;    (process-rectangle translate a-rect)))

;; tests
;(check-expect (translate-rectangle (make-rectangle (make-posn 0 0) 10 20 'red) 10)
;              (make-rectangle (make-posn 10 0) 10 20 'red))

;; ex 21.4.3 -- ????
;; 구지 하자면 이렇게??? 암만 봐도 아닌거 같은데. - FAIL!!!

;; process-shape : (listof [shape -> X]), shape -> X
;(define (process-shape fns s)
;  (cond
;    [(circle? s) ((first fns) s)]
;    [(rectangle? s) ((second fns) s)]))

;; draw-shape : shape -> true
;; draw a shape s
;(define (draw-shape s)
;  (process-shape (list draw-a-circle draw-a-rectangle) s))
(define (draw-shape s)
  (cond
    [(circle? s) (draw-a-circle s)]
    [(rectangle? s) (draw-a-rectangle s)]
    [(line? s) (draw-a-line s)]))

;; clear-shape : shape -> true
;; clear a shape s
;(define (clear-shape s)
;  (process-shape (list clear-a-circle clear-a-rectangle) s))
(define (clear-shape s)
  (cond
    [(circle? s) (clear-a-circle s)]
    [(rectangle? s) (clear-a-rectangle s)]
    [(line? s) (clear-a-line s)]))

;; fun-for-shape : a-shape -> ???
(define (fun-for-shape a-shape)
  (cond
    [(circle? a-shape) ...]
    [(rectangle? a-shape) ...]))

;; translate-shape : shape, number -> shape
;(define (translate-shape a-shape delta)
;  (local ((define (t1 s) (translate-circle s delta))
;          (define (t2 s) (translate-rectangle s delta)))
;    (process-shape (list t1 t2) a-shape)))
;(define (translate-shape a-shape delta)
;  (cond
;    [(circle? a-shape) (translate-circle a-shape delta)]
;    [(rectangle? a-shape) (translate-rectangle a-shape delta)]))

;; ex 21.4.4
;; draw-losh : shapes -> true
(define (draw-losh losh)
  (andmap draw-shape losh))

;; clear-losh : shapes -> true
(define (clear-losh losh)
  (andmap clear-shape losh))

;; translate-losh : shapes number -> shapes
(define (translate-losh losh dx dy)
  (local ((define (translate s)
            (translate-shape s dx dy)))
    (map translate losh)))

;; ex 21.4.5
;; translate-circle : circle, number, number -> circle
(define (translate-circle s dx dy)
  (local ((define (translate p r c)
            (make-circle
             (make-posn (+ dx (posn-x p))
                        (+ dy (posn-y p)))
             r c)))
    (process-circle translate s)))

;; tests
(check-expect (translate-circle (make-circle (make-posn 0 0) 10 'red) 10 5)
              (make-circle (make-posn 10 5) 10 'red))

;; translate-rectangle : rectangle, number, number -> rectangle
(define (translate-rectangle a-rect dx dy)
  (local ((define (translate a-posn width height color)
            (make-rectangle
             (make-posn (+ dx (posn-x a-posn))
                        (+ dy (posn-y a-posn)))
             width height color)))
    (process-rectangle translate a-rect)))

;; tests
(check-expect (translate-rectangle (make-rectangle (make-posn 0 0) 10 20 'red) 10 5)
              (make-rectangle (make-posn 10 5) 10 20 'red))

;; translate-shape : shape, number, number -> shape
;(define (translate-shape a-shape dx dy)
;  (local ((define (t1 s) (translate-circle s dx dy))
;          (define (t2 s) (translate-rectangle s dx dy)))
;    (process-shape (list t1 t2) a-shape)))
(define (translate-shape a-shape dx dy)
  (cond
    [(circle? a-shape) (translate-circle a-shape dx dy)]
    [(rectangle? a-shape) (translate-rectangle a-shape dx dy)]
    [(line? a-shape) (translate-line a-shape dx dy)]))

;; tests
(check-expect (translate-shape (make-circle (make-posn 0 0) 10 'red) 10 5)
              (make-circle (make-posn 10 5) 10 'red))
(check-expect (translate-shape (make-rectangle (make-posn 0 0) 10 20 'red) 10 5)
              (make-rectangle (make-posn 10 5) 10 20 'red))
(check-expect (translate-shape (make-line (make-posn 0 0) (make-posn 10 10) 'red) 8 5)
              (make-line (make-posn 8 5) (make-posn 18 15) 'red))

;; line
(define-struct line (start end color))
;; A rectangle is a structure:
;;          (make-line s e c)
;;    where s and e are posns, and c is a color.

;; process-line : [posn posn color -> X] line -> X
(define (process-line fn a-line)
  (fn (line-start a-line)
      (line-end a-line)
      (line-color a-line)))

;; draw-a-line : line -> true
;; draw a line a-line
(define (draw-a-line a-line)
  (process-line draw-solid-line a-line))

;; clear-a-line : line -> true
;; clear a line a-line
(define (clear-a-line a-line)
  (process-rectangle clear-solid-line a-line))

;; translate-line : line, number, number -> line
(define (translate-line a-line dx dy)
  (local ((define (translate start end color)
            (make-line
             (make-posn (+ dx (posn-x start))
                        (+ dy (posn-y start)))
             (make-posn (+ dx (posn-x end))
                        (+ dy (posn-y end)))
             color)))
    (process-line translate a-line)))

;; tests
(check-expect (translate-line (make-line (make-posn 0 0) (make-posn 10 10) 'red) 8 5)
              (make-line (make-posn 8 5) (make-posn 18 15) 'red))

(define LUNAR (list
               (make-circle (make-posn 10 10) 10 'blue)
               (make-rectangle (make-posn 0 20) 20 10 'red)
               (make-line (make-posn 6 30) (make-posn 0 40) 'black)
               (make-line (make-posn 14 30) (make-posn 20 40) 'black)
               ))

(define (lunar-lander dy lunar)
  (draw-losh
   (translate-losh lunar 0 dy)))

;; ????
;(start 500 100)
;(draw-losh LUNAR)
;(control-up-down LUNAR 10 lunar-lander draw-losh)
