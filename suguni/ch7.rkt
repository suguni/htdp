;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ch 7

;; ch 7.1

;; ex 7.1.1
(define-struct star (last first dob ssn))
(check-expect (number? (make-posn 2 3)) false) ;; false
(number? (+ 12 100)) ;; true
(posn? 23) ;; false
(posn? (make-posn 23 3)) ;; true
(star? (make-posn 23 3)) ;; false

;; Data definition:
(define-struct square (nw length))
(define-struct circle (center radius))
;; A shape is either
;; 1. a circle structure: (make-circle p s)
;;    where p is a posn and s is a number; or
;; 2. a square structure: (make-square p s)
;;    where p is a posn and s is a number. 

;; Contract, Purpose, Header:
;; perimeter : shape -> number
;; to compute the perimeter of a-shape

;; Examples: see tests

;; Template:
;; (define (f a-shape)
;;   (cond
;;     [(square? a-shape)
;;      ... (square-nw a-shape) ...  (square-length a-shape) ...]
;;     [(circle? a-shape)
;;      ... (circle-center a-shape) ... (circle-radius a-shape) ...]))

;; Definition:
(define (perimeter a-shape)
  (cond
    [(square? a-shape)
     (* (square-length a-shape) 4)]
    [(circle? a-shape)
     (* 2 pi (circle-radius a-shape))]))

;; Tests:
;; (= (perimeter (make-square ... 3)) 12)
;; (= (perimeter (make-circle ... 1)) (* 2 pi))

;; ex 7.1.2

;; Tests:
(perimeter (make-square (make-posn 20 20) 3)) ;; = 12
(perimeter (make-square (make-posn 2 20) 3)) ;; = 12
(perimeter (make-circle (make-posn 10 99) 1)) ;; = 6.28

;; ex 7.1.3

;; area : shape -> number
(define (area a-shape)
  (cond
    [(square? a-shape)
     (sqr (square-length a-shape))]
    [(circle? a-shape)
     (* pi (sqr (circle-radius a-shape)))]))

;; Tests:
(area (make-square (make-posn 20 20) 3)) ;; = 9
(area (make-square (make-posn 2 20) 3)) ;; = 9
(area (make-circle (make-posn 10 99) 1)) ;; = 3.14

;; template이 정확히 뭐야?
;; => 사용가능하다.

;; ch 7.2

;; ex 7.2.1

;; Data definition:
(define-struct spider (legs space))
(define-struct elephant (space))
(define-struct monkey (intelligence space))
;; A animal is either
;; 1. a spider structure: (make-spider l s)
;;    where l and s are numbers; or
;; 2. a elephant structure: (make-elephant s)
;;    where s is a number; or
;; 3. a monkey structure: (make-monkey i s)
;;    where i and s are numbers

;; Contract, Purpose, Header:
;; fits? : animal, number -> boolean
;; Determines whether the cage is large enough for the animal.

;; Examples: see tests

;; Template:
;; (define (f a-animal)
;;   (cond
;;     [(spider? a-animal)
;;      ... (spider-legs a-animal) ... (spider-space a-animal) ...]
;;     [(elephant? a-animal)
;;      ... (elephant-space a-animal) ...]
;;     [(monkey? a-animal)
;;      ... (monkey-intelligence a-animal) ... (monkey-space a-animal) ...]))

;; Auxiliary functions
(define (cube n)
  (* n n n))

;; Definition:
;(define (fits? a-animal cage)
;  (cond
;    [(spider? a-animal)
;     (> cage (cube (spider-space a-animal)))]
;    [(elephant? a-animal)
;     (> cage (cube (elephant-space a-animal)))]
;    [(monkey? a-animal)
;     (> cage (cube (monkey-space a-animal)))]))
;; space를 너무 복잡하게 생각한듯.
(define (fits? a-animal cage)
  (cond
    [(spider? a-animal)
     (> cage (spider-space a-animal))]
    [(elephant? a-animal)
     (> cage (elephant-space a-animal))]
    [(monkey? a-animal)
     (> cage (monkey-space a-animal))]))

;; Tests:
(check-expect (fits? (make-spider ... 3) 50) true)
(check-expect (fits? (make-elephant 5) 200) true)
(check-expect (fits? (make-monkey ... 4) 100) true)

;; ex 7.2.2

;; Data definition:
;; (define-struct bus (...))
;; (define-struct limo (...))
;; (define-struct car (...))
;; (define-struct subway (...))

;; A transportation is either
;; 1. a bus structure: (make-bus ...)
;;    where ... are ...; or
;; 2. a limo structure: (make-limo ...)
;;    where ... are ...; or
;; 3. a car structure: (make-car ...)
;;    where ... are ...; or
;; 4. a subway structure: (make-subway ...)
;;    where ... are ...; or

;; Template:
;; (define (f a-transportation)
;;   (cond
;;     [(bus? a-transportation)
;;      ... (bus-... a-transportation) ...]
;;     [(limo? a-transportation)
;;      ... (limo-... a-transportation) ...]
;;     [(car? a-transportation)
;;      ... (car-... a-transportation)  ...]
;;     [(subway? a-transportation)
;;      ... (subway-... a-transportation)]))

;; ch 7.3

;; ex 7.3.1

(define-struct rectangle (ul width height))
;; A rectangle is a structure:
;;          (make-rectangle p w h)
;;    where p is a posn, w and h a number.

;; A shape is either
;; 1. a circle, or
;; 2. a square, or
;; 3. a rectangle.

;; Final Definitions: 
;; perimeter : shape  ->  number
;; to compute the perimeter of a-shape
(define (perimeter-2 a-shape)
  (cond
    [(circle? a-shape)
     (perimeter-circle a-shape)]
    [(square? a-shape)
     (perimeter-square a-shape)]
    [(rectangle? a-shape)
     (perimeter-rectangle a-shape)]))

;; perimeter-circle : circle  ->  number
;; to compute the perimeter of a-circle
(define (perimeter-circle a-circle)
  (* (* 2 (circle-radius a-circle)) pi))

;; perimeter-square : square  ->  number
;; to compute the perimeter of a-square
(define (perimeter-square a-square)
  (* (square-length a-square) 4))

;; perimeter-rectangle : rectangle -> number
;; to compute the perimeter of a-rectangle
(define (perimeter-rectangle a-rectangle)
  (* (+ (rectangle-width a-rectangle)
        (rectangle-height a-rectangle)) 2))

;; (perimeter-2 (make-rectangle (make-posn 10 10) 20 40)) ;; => 120

;; ch 7.4
;; ch7.4.rkt

;; ch 7.5

;; ex 7.5.1
(define (area-of-disk r)
  (* r r pi))

;; checked-area-of-disk : Scheme-value  ->  number
;; to compute the area of a disk with radius v, 
;; if v is a positive number
(define (checked-area-of-disk v)
  (cond
    [(and (number? v) (> v 0)) (area-of-disk v)]
    [else (error 'checked-area-of-disk "positive number expected")]))

;; 2011/12/6, error test
(check-error (checked-area-of-disk "number"))


;; ex 7.5.2
#|
(define (profit ticket-price)
  (cond
    [(and (number? ticket-price) (> ticket-price ))
     (- (revenue ticket-price)
        (cost ticket-price))]
    [else (error 'profit "positive number expected")]))
|#

;; is-between-5-6? : number  ->  boolean
;; to determine whether n is between 5 and 6 (exclusive)
(define (is-between-5-6? n)
  (cond
    [(number? n)
     (and (< 5 n) (< n 6))]
    [else (error 'is-between-5-6? "number expected")]))

(define (reply s)
  (cond
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]
    [else (error 'reply "symbol ('GoodMorning or 'HowAreYou? or 'GoodAfternoon or 'GoodEvening) expected")]))

(define (distance-to-0 a-posn)
  (cond
    [(posn? a-posn)
     (sqrt
      (+ (sqr (posn-x a-posn))
         (sqr (posn-y a-posn))))]
    [else (error 'distance-to-0 "posn expected")]))

;; Final Definitions: 
;; perimeter : shape  ->  number
;; to compute the perimeter of a-shape
(define (perimeter-3 a-shape)
  (cond
    [(circle? a-shape)
     (perimeter-circle a-shape)]
    [(square? a-shape)
     (perimeter-square a-shape)]
    [else (error 'perimeter "shape(circle or square) expected")]))

;; ex 7.5.3
(define-struct vec (x y))
(define (checked-make-vec x y)
  (cond
    [(and (and (number? x) (> x 0))
          (and (number? y) (> y 0)))
     (make-vec x y)]
    [else (error 'checked-make-vec "positive number expected")]))
