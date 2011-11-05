;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch3) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor mixed-fraction #f #t none #f ((lib "convert.ss" "teachpack" "htdp")))))
;; ch 3.1

;; ex 3.1.1 ~ 3.1.2
;; profit : number -> number
(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

;; revenue : number -> number
(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

;; cost : number -> number
;; e.g.
;; (cost 3) -> 180 + 420 * 0.04 -> 196.8
;; (cost 4) -> 180 + 270 * 0.04 -> 190.8
;; (cost 5) -> 180 + 120 * 0.04 -> 184.8
(define (cost ticket-price)
  (+ 180 (* (attendees ticket-price) 0.04)))

;; attendees : number -> number
;; e.g.
;; (attendees 3) -> 420
;; (attendees 4) -> 270
;; (attendees 5) -> 120
;; 120 + (5 - ticket-price) * 150
(define (attendees ticket-price)
  (+ 120 (* (- 5 ticket-price) 150)))

;; max profit price = 3.00

(define (profit-2 price)
  (- (* (+ 120
           (* (/ 15 .10)
              (- 5.00 price)))
        price)
     (+ 180
        (* .04
           (+ 120
              (* (/ 15 .10)
                 (- 5.00 price)))))))

;; ex 3.1.3
;; > (profit 3)
;; 1063.2
;; > (profit 4)
;; 889.2
;; > (profit 5)
;; 415.2
;; > (profit-2 3)
;; 1063.2
;; > (profit-2 4)
;; 889.2
;; > (profit-2 5)
;; 415.2

;; ex 3.1.4
;; profit-2 : number -> number
(define (profit-3 ticket-price)
  (- (revenue ticket-price)
     (cost-3 ticket-price)))

;; cost : number -> number
;; e.g.
;; (cost 3) -> 420 * 1.5
;; (cost 4) -> 270 * 1.5
;; (cost 5) -> 120 * 1.5
(define (cost-3 ticket-price)
  (* (attendees ticket-price) 1.50))

;; > (profit-3 3.00)
;; 630
;; > (profit-3 4.00)
;; 675
;; > (profit-3 5.00)
;; 420

;; ch 3.2
;; ex 3.2.1
(define fixed-cost 180)
(define cost-per-attendee 0.04)
(define rel-base-price 5.00)
(define rel-base-attendee 120)
(define rel-change-price 0.10)
(define rel-change-attendee 15)

; (define (cost ticket-price)
;   (+ fixed-cost (* (attendees ticket-price)
;                    cost-per-attendee)))
; 
; (define (attendees ticket-price)
;   (+ rel-base-attendee (* (- rel-base-price ticket-price)
;                           (/ rel-change-attendee rel-change-price))))


;; ch 3.3
;; ex 3.3.1
(define cm/in 2.54)
(define in/ft 12)
(define ft/yd 3)
(define yd/rd 5.5)
(define rd/fl 40)
(define fl/ml 8)

;; inches->cm : number -> number
;; 1 inch = 2.54 cm
(define (inches->cm n)
  (* n 2.54))

;; feet->inches : number -> number
;; 1 feet = 12 in.
(define (feet->inches n)
  (* n 12))

;; yards->feet : number -> number
;; 1 year = 3 ft.
(define (yards->feet n)
  (* n 3))

;; rods->yards : number -> number
;; 1 rod = 5 1/2 yd.
(define (rods->yards n)
  (* n 5.5))

;; furlongs->rods : number -> number
;; 1 furlong = 40 rd.
(define (furlongs->rods n)
  (* n 40))

;; miles->furlongs : number -> number
;; 1 mile = 8 fl.
(define (miles->furlongs n)
  (* n 40))

;; feet->cm : number -> number
;; 1 feet = 12 in. = 12 * (2.54 cm)
(define (feet->cm n)
  (inches->cm
   (feet->inches n)))

;; yards->cm : number -> number
;; 1 yard = 3 ft
(define (yards->cm n)
  (feet->cm
   (yards->feet n)))

;; rods->inches : number -> number
;; 1 rd. = 5.5 yd. , 1 yd. = 3 ft., 1 ft. = 12 in.
(define (rods->inches n)
  (feet->inches
   (yards->feet
    (rods->yards n))))

;; miles->feet : number -> number
;; 1 ml. = 8 fl. 1 fl. = 40 rd., 1 rd. = 5.5 yd. , 1 yd. = 3 ft.
(define (miles->feet n)
  (yards->feet
   (rods->yards
    (furlongs->rods
     (miles->furlongs n)))))

;; ex 3.3.2

(define PI 3.141592)
(define (area-of-disk radius)
  (* PI radius radius))
(define (perimeter-of-disk radius)
  (* 2 PI radius))
         
;; volume-cylinder : number(radius), number(height) -> number(volume)
;; volume = pi * radius^2 * height
(define (volume-cylinder radius height)
  (* (area-of-disk radius) height))

;; ex 3.3.3
(define (area-cylinder radius height)
  (+ (* 2 (area-of-disk radius))
     (* height (perimeter-of-disk radius))))
;; (* height (perimeter-of-disk radius))-원통의 옆면 너비, 이것도 다른 함수로 만들어야 할까?

;; ex 3.3.4
;; area-of-disk, perimeter-of-disk는 앞에서 정의되어 있음
(define (area-of-ring outer inner)
  (- (area-of-disk outer)
     (area-of-disk inner)))
(define (area-pipe inner-radius length thickness)
  (+ (* length (perimeter-of-disk (+ inner-radius thickness)))
     (* 2 (area-of-ring (+ inner-radius thickness) inner-radius))))

;; ex 3.3.5
(define (velocity g t)
  (* g t))
(define (height g t)
  (* 1/2 (velocity g t) t))

;; ex 3.3.6
(define (Fahrenheit->Celsius temp)
  (* (- temp 32) 5/9))
(define (Celsius->Fahrenheit temp)
  (+ (* 9/5 temp) 32))

;; I : number->number
(define (I f)
  (Celsius->Fahrenheit (Fahrenheit->Celsius f)))

(I 32)
;; Celsius->Fahrenheit와 Fahrenheit->Celsius는 역함수 관계임
