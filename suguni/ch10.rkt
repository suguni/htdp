;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch10) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp") (lib "arrow.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp") (lib "arrow.ss" "teachpack" "htdp")))))
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
(check-error (hours->wages-2 (cons 10 (cons 20 (cons 150 (cons 80 empty))))))

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
(check-expect (convertFC empty) empty)
(check-expect (convertFC (cons 32 empty)) (cons 0 empty))
(check-expect (convertFC (cons 32 (cons 41 empty))) (cons 0 (cons 5 empty)))

;; ex 10.1.4
;; convert-euro : list-of-numbers -> list-of-numbers
(define (convert-euro lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (* 1.22 (first lon))
           (convert-euro (rest lon)))]))
;; test
(check-expect (convert-euro (cons 1 (cons 2 empty)))
              (cons 1.22 (cons 2.44 empty)))

;; convert-euro-1 : number, list-of-numbers -> list-of-numbers
(define (convert-euro-1 ratio lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons (* ratio (first lon))
           (convert-euro-1 ratio (rest lon)))]))
;; test
(check-expect (convert-euro-1 1.5 (cons 1 (cons 2 empty)))
              (cons 1.5 (cons 3 empty)))

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
(check-expect (eliminate-exp 1.0 (cons 2.95 (cons .95 (cons 1.0 (cons 5 empty)))))
              (cons .95 (cons 1.0 empty)))

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
(check-expect (name-robot empty) empty)
(check-expect (name-robot (cons 'kitty empty))
              (cons 'kitty empty))
(check-expect (name-robot (cons 'kitty (cons 'robot (cons 'mazinga (cons 'robot empty)))))
              (cons 'kitty (cons 'r2d2 (cons 'mazinga (cons 'r2d2 empty)))))

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
(check-expect (substitute 'r2d2 'robot empty) empty)
(check-expect (substitute 'magic 'kitty (cons 'kitty empty))
              (cons 'magic empty))
(check-expect (substitute 'r2d2 'mazinga (cons 'kitty (cons 'robot (cons 'mazinga (cons 'robot empty)))))
              (cons 'kitty (cons 'robot (cons 'r2d2 (cons 'robot empty)))))

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
(check-expect (recall 'robot (cons 'robot (cons 'doll (cons 'dress empty))))
              (cons 'doll (cons 'dress empty)))

;; ex 10.1.8
(define (how-many a b c)
  (cond
    [(= a 0) -1]
    [(> (* b b) (* 4 a c)) 2]
    [(= (* b b) (* 4 a c)) 1]
    [(< (* b b) (* 4 a c)) 0]))

;; Test set
(check-expect (how-many 0 2 1) -1)
(check-expect (how-many 1 0 -1) 2)
(check-expect (how-many 2 4 2) 1)
(check-expect (how-many 1 0 1) 0)

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
(check-expect (quadratic-roots 0 2 1) 'degenerate)
(check-expect (quadratic-roots 1 0 -1) (cons 1 (cons -1 empty)))
(check-expect (quadratic-roots 2 4 2) -1)
(check-expect (quadratic-roots 1 0 1) 'none)

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

;; tests
(check-expect (controller 103)
              (cons 1 (cons 'dollar (cons 'and (cons 3 (cons 'cents empty))))))

;; Questions
;; (controller 100) ;;=> (cons 1 (cons 'dollar empty)) or (cons 1 (cons 'dollar (cons 'and (cons 0 (cons 'cent empty)))))
;; (controller 0) ;; => ???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ch 10.2

(define-struct ir (name price))

;; inventory examples
;; empty
;; (cons (make-ir 'doll 17.95)
;;             empty)
;; (cons (make-ir 'robot 22.05)
;;       (cons (make-ir 'doll 17.95)
;;            empty))

;; sum : inventory -> number
;; to compute the sum of prices on an-inv
(define (sum an-inv)
  (cond
    [(empty? an-inv) 0]
    [else
     (+ (ir-price (first an-inv))
        (sum (rest an-inv)))]))


;; ex 10.2.1
;; contains-doll? : list-of-inv -> boolean
;; to determine whether the symbol 'doll occurs on ir
(define (contains-doll? an-inv)
  (cond
   [(empty? an-inv) false]
   [else
    (cond
     [(symbol=? (ir-name (first an-inv)) 'doll) true]
     [else (contains-doll? (rest an-inv))])]))
;; test
;; true
(check-expect (contains-doll? (cons (make-ir 'robot 22.05)
                                    (cons (make-ir 'doll 17.95)
                                          empty))) true)
;; false
(check-expect (contains-doll? (cons (make-ir 'robot 22.05)
                                    (cons (make-ir 'lego 17.95)
                                          empty))) false)

;; ex 10.2.2
;; ch10-ex10.2.2.rkt

;; ex 10.2.3
;; contains-doll? : list-of-inv -> boolean
;; to determine whether the symbol 'doll occurs on ir
(define (price-of toy an-inv)
  (cond
    [(empty? an-inv) false]
    [else
     (cond
       [(symbol=? (ir-name (first an-inv)) toy) (ir-price (first an-inv))]
       [else (price-of toy (rest an-inv))])]))

;; test
(check-expect (price-of 'doll (cons (make-ir 'robot 22.05)
                                    (cons (make-ir 'doll 17.95)
                                          empty)))
              17.95)

;; ex 10.2.3

;; phone record
(define-struct pr (name pn))

;; whose-number : phone-number phone-directory -> name
(define (whose-number phone-number phone-directory)
  (cond
    [(empty? phone-directory) empty]
    [else
     (cond
       [(symbol=? (pr-pn (first phone-directory)) phone-number)
        (pr-name (first phone-directory))]
       [else
        (whose-number phone-number (rest phone-directory))])]))

;; test data
(define my-phone-dir (cons (make-pr 'steve-yu '010-4206-2688)
                           (cons (make-pr 'bizen-lee '010-4847-0337)
                                 empty)))
;; test
(check-expect (whose-number '010-4206-2688 my-phone-dir) 'steve-yu)
(check-expect (whose-number '010-1234-2688 my-phone-dir) empty)

;; phone-number : name phone-directory -> phone-number
(define (phone-number name phone-directory)
  (cond
    [(empty? phone-directory) empty]
    [else
     (cond
       [(symbol=? (pr-name (first phone-directory)) name)
        (pr-pn (first phone-directory))]
       [else
        (phone-number name (rest phone-directory))])]))

;; test
(check-expect (phone-number 'bizen-lee my-phone-dir) '010-4847-0337)
(check-expect (phone-number 'steve-jobs my-phone-dir) empty)

;; p171

(define my-inv
  (cons (make-ir 'dagger .95)
        (cons (make-ir 'Barbie 17.95)
              (cons (make-ir 'key-chain .55)
                    (cons (make-ir 'robot 22.05)
                          empty)))))

;; extract1 : inventory -> inventory
;; to create an inventory from an-inv for all those items that cost less than $1
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) empty]
    [else
     (cond
       [(<= (ir-price (first an-inv)) 1.00)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

;; test
(check-expect (extract1 my-inv) ;; =>
              (cons (make-ir 'dagger .95)
                    (cons (make-ir 'key-chain .55)
                          empty)))

;; ex 10.2.5
;; extract>1 : inventory => inventory
(define (extract>1 an-inv)
  (cond
    [(empty? an-inv) empty]
    [else
     (cond
       [(> (ir-price (first an-inv)) 1.00)
        (cons (first an-inv) (extract>1 (rest an-inv)))]
       [else (extract>1 (rest an-inv))])]))

;; test
(check-expect (extract>1 my-inv) ;; =>
              (cons (make-ir 'Barbie 17.95)
                    (cons (make-ir 'robot 22.05)
                          empty)))

;; ex 10.2.6
;; inventory1
(define-struct ir1 (name price))

;; extract1-1 : inventory -> inventory1
;; to create an inventory from an-inv for all those items that cost less than $1
(define (extract1-1 an-inv)
  (cond
    [(empty? an-inv) empty]
    [else
     (cond
       [(<= (ir-price (first an-inv)) 1.00)
        (cons
         (make-ir1 (ir-name (first an-inv)) (ir-price (first an-inv)))
         (extract1-1 (rest an-inv)))]
       [else (extract1-1 (rest an-inv))])]))

(check-expect (extract1-1 my-inv)
              (cons (make-ir1 'dagger .95)
                    (cons (make-ir1 'key-chain .55)
                          empty)))

;; ex 10.2.7
;; raise-prices : inventory -> inventory
(define (raise-prices an-inv)
  (cond
    [(empty? an-inv) empty]
    [else
     (cons 
      (make-ir (ir-name (first an-inv)) (* (ir-price (first an-inv)) 1.05))
      (raise-prices (rest an-inv)))]))

;; test
(check-expect (raise-prices my-inv) ;; =>
              (cons (make-ir 'dagger .9975)
                    (cons (make-ir 'Barbie 18.8475)
                          (cons (make-ir 'key-chain .5775)
                                (cons (make-ir 'robot 23.1525)
                                      empty)))))

;; ex 10.2.8
;; recall-1 : symbol inventory -> inventory
(define (recall-1 ty lon)
  (cond
    [(empty? lon) empty]
    [else
     (cond
       [(symbol=? (ir-name (first lon)) ty) (recall-1 ty (rest lon))]
       [else (cons (first lon) (recall-1 ty (rest lon)))])]))

;; test
(check-expect (recall-1 'robot my-inv)
              (cons (make-ir 'dagger .95)
                    (cons (make-ir 'Barbie 17.95)
                          (cons (make-ir 'key-chain .55)
                                empty))))

;; ex 10.2.9

;; name-robot-1 : inventory -> inventory
(define (name-robot-1 los)
  (cond
    [(empty? los) empty]
    [else
     (cons
      (cond
        [(symbol=? (ir-name (first los)) 'robot)
         (make-ir 'r2d2 (ir-price (first los)))]
        [else (first los)])
      (name-robot-1 (rest los)))]))

;; test
(check-expect (name-robot-1 my-inv)
              (cons (make-ir 'dagger .95)
                    (cons (make-ir 'Barbie 17.95)
                          (cons (make-ir 'key-chain .55)
                                (cons (make-ir 'r2d2 22.05)
                                      empty)))))

;; substitute-1 : symbol symbol inventory -> inventory
(define (substitute-1 new old los)
  (cond
    [(empty? los) empty]
    [else
     (cons
      (cond
        [(symbol=? (ir-name (first los)) old)
         (make-ir new (ir-price (first los)))]
        [else (first los)])
      (substitute-1 new old (rest los)))]))

;; test
(check-expect (substitute-1 'Cabbage 'Barbie my-inv)
              (cons (make-ir 'dagger .95)
                    (cons (make-ir 'Cabbage 17.95)
                          (cons (make-ir 'key-chain .55)
                                (cons (make-ir 'robot 22.05)
                                      empty)))))

;; ex 10.3.1

;; Shape definitions from ex 7.4.1
;; Data Definitions:
(define-struct circle (center radius color))
;; A circle is a structure:
;;          (make-circle p s c)
;;    where p is a posn, s is a number and c is a color;

(define-struct rectangle (nw width height color))
;; A rectangle is a structure:
;;          (make-rectangle p w h c)
;;    where p is a posn, w and h a number and c is a color.

(define FACE
  (cons (make-circle (make-posn 50 50) 40 'red)
        (cons (make-rectangle (make-posn 30 20) 5 5 'blue)
              (cons (make-rectangle (make-posn 65 20) 5 5 'blue)
                    (cons (make-rectangle (make-posn 40 75) 20 10 'red)
                          (cons (make-rectangle (make-posn 45 35) 10 30 'blue)
                                empty))))))

;; fun-for-shape : a-shape -> ???
(define (fun-for-shape a-shape)
  (cond
    [(circle? a-shape) ...]
    [(rectangle? a-shape) ...]))

;; fun-for-shape : a-shape -> ???
(define (fun-for-losh list-of-shapes)
  (cond
    [(empty? list-of-shapes) true]
    [else
     (... (fun-for-shape (first list-of-shapes)) ...
          ... (fun-for-losh (rest list-of-shapes)) ...)]))

;; ex 10.3.2

;; draw-shape : shape -> true
;; draw a shape s
(define (draw-shape s)
  (cond
    [(circle? s) (draw-a-circle s)]
    [(rectangle? s) (draw-a-rectangle s)]))

;; draw-a-circle : circle -> true
;; draw a circle s
(define (draw-a-circle s)
  (draw-circle (circle-center s)
               (circle-radius s)
               (circle-color s)))

;; draw-a-rectangle : rectangle -> true
;; draw a rectangle s
(define (draw-a-rectangle s)
  (draw-solid-rect (rectangle-nw s)
                   (rectangle-width s)
                   (rectangle-height s)
                   (rectangle-color s)))

;; draw-losh : shapes -> true
(define (draw-losh losh)
  (cond
    [(empty? losh) true]
    [else
     (and (draw-shape (first losh))
          (draw-losh (rest losh)))]))

;; (start 300 100)
;; (draw-losh FACE)

;; ex 10.3.3

;; translate-shape : shape, number -> shape
(define (translate-shape delta a-shape)
  (cond
    [(circle? a-shape) (translate-circle delta a-shape)]
    [(rectangle? a-shape) (translate-rectangle delta a-shape)]))

;; translate-circle : circle, number -> shape
(define (translate-circle d s)
  (make-circle
   (make-posn (+ d  (posn-x (circle-center s)))
              (posn-y (circle-center s)))
   (circle-radius s)
   (circle-color s)))

;; translate-rectangle : rectangle, number -> shape
(define (translate-rectangle d s)
  (make-rectangle
   (make-posn (+ d (posn-x (rectangle-nw s)))
              (posn-y (rectangle-nw s)))
   (rectangle-width s)
   (rectangle-height s)
   (rectangle-color s)))

;; translate-losh : number shapes -> shapes
(define (translate-losh delta losh)
  (cond
    [(empty? losh) empty]
    [else
     (cons (translate-shape delta (first losh))
           (translate-losh delta (rest losh)))]))

;; test
;; (start 300 100)
;; (draw-losh FACE)
;; (define FACE-D (translate-losh 100 FACE))
;; (draw-losh FACE-D)

;; ex 10.3.4

;; clear-shape : shape -> true
;; clear a shape s
(define (clear-shape s)
  (cond
    [(circle? s) (clear-a-circle s)]
    [(rectangle? s) (clear-a-rectangle s)]))

;; clear-a-circle : circle -> true
(define (clear-a-circle s)
  (clear-circle (circle-center s)
                (circle-radius s)
                (circle-color s)))

;; clear-a-rectangle : rectangle -> true
(define (clear-a-rectangle s)
  (clear-solid-rect (rectangle-nw s)
                    (rectangle-width s)
                    (rectangle-height s)
                    (rectangle-color s)))

;; clear-losh : shapes -> true
(define (clear-losh losh)
  (cond
    [(empty? losh) true]
    [else
     (and (clear-shape (first losh))
          (clear-losh (rest losh)))]))

;; test
;; (start 300 100)
;; (draw-losh FACE)
;; (define FACE-D (translate-losh FACE 100))
;; (clear-losh FACE)
;; (draw-losh FACE-D)

;; ex 10.3.5
;; draw-and-clear-picture : picture -> true
(define (draw-and-clear-picture picture)
  (and
   (draw-losh picture)
   (sleep-for-a-while 0.05)
   (clear-losh picture)))

;; test
;; (start 300 100)
;; (draw-and-clear-picture FACE)

;; ex 10.3.6
;; move-picture : delta picture -> picture
(define (move-picture delta picture)
  (cond
    [(draw-and-clear-picture picture) (translate-losh delta picture)]
    [else picture]))

;; test
;(start 500 100)
;(draw-losh
; (move-picture -5
;               (move-picture 23
;                             (move-picture 10 FACE))))
