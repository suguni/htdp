;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch9) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")))))
;; ch 9

;; ex 9.1.1
;; 1.
(cons 'Neptune
      (cons 'Uranus 
            (cons 'Saturn 
                  (cons 'Jupiter 
                        (cons 'Mars 
                              (cons 'Earth 
                                    (cons 'Venus 
                                          (cons 'Mercury empty))))))))
;; 2.
(cons 'ice-cream
      (cons 'cheese 
            (cons 'juice 
                  (cons 'water 
                        (cons 'bread 
                              (cons 'beans 
                                    (cons 'fried-potato-chip 
                                          (cons 'stake empty))))))))
;; 3.
(cons 'red (cons 'green (cons 'blue empty)))

;; p.138
;; add-up-3: list-of-3-numbers -> number
;; examples:
;; (= (add-up-3 (cons 2 (cons 1 (cons 3 empty)))) 6)
;; (= (add-up-3 (cons 0 (cons 1 (cons 0 empty)))) 1)
;(define (add-up-3 a-list-of-3-numbers)
;  ... (first a-list-of-3-numbers) ...
;  ... (first (rest a-list-of-3-number)) ...
;  ... (first (rest (rest a-list-of-3-number))) ...)

;; ex 9.1.2
(define l (cons 10 (cons 20 (cons 5 empty))))
;; 1.
(rest l)
(cons 20 (cons 5 empty))
;; 2.
(first (rest l))
20
;; 3.
(rest (rest l))
(cons 5 empty)
;; 4.
(first (rest (rest l)))
5
;; 5.
(rest (rest (rest l)))
empty

;; ex 9.1.3
'ex-9.1.3
(define (add-up-3 a-list-of-3-numbers)
  (+ (first a-list-of-3-numbers)
     (first (rest a-list-of-3-numbers))
     (first (rest (rest a-list-of-3-numbers)))))
(= (add-up-3 (cons 2 (cons 1 (cons 3 empty)))) 6)
(= (add-up-3 (cons 0 (cons 1 (cons 0 empty)))) 1)

;; 3D length
(define (distance-to-0-for-3 a-list-of-3-numbers)
  (sqrt
   (+ (sqr (first a-list-of-3-numbers))
      (sqr (first (rest a-list-of-3-numbers)))
      (sqr (first (rest (rest a-list-of-3-numbers)))))))
(= (distance-to-0-for-3 (cons 1 (cons 1 (cons 1 empty)))) (sqrt 3))

;; ex 9.1.4
;; contains-2-doll? a-list-of-2-symbols -> boolean
(define (contains-2-doll? a-list-of-2-symbols)
  (or
   (symbol=? (first a-list-of-2-symbols) 'doll)
   (symbol=? (first (rest a-list-of-2-symbols)) 'doll)))

(define l1 (cons 'doll (cons 'dull empty)))
(define l2 (cons 'boy (cons 'girl empty)))
'ex-9.1.4
(eq? (contains-2-doll? l1) true)
(eq? (contains-2-doll? l2) false)

;; ex 9.2.1 ???
;; 아래는 모두 list-of-symbol class에 속한다.
empty
;; empty는 list-of-symbol 이다.
(cons 'ball empty)
;; 'ball은 symbol이고 empty는 list-of-symbol 이다.
(cons 'arrow (cons 'ball empty))
;; 'arrow는 symbol이고 (cons 'ball empty)은 위에서와 같이
;; list-of-symbol 이므로 전체는 list-of-symbol 이다.
(cons 'clown empty)
;; 'clown은 symbol이고 empty는 list-of-symbol 이다.
(cons 'bow (cons 'arrow empty))
;; 'bow는 symbol이고 (cons 'arrow empty)는 list-of-symbol 이다.
(cons 'clown (cons 'doll (cons 'arrow (cons 'ball empty))))
;; 위와 같이 봐서 list-of-symbol 이다.

;; ex 9.2.2
;; 두개의 symbol로 구성된 모든 리스트는 list-of-symbols인가?
;; 두개의 symbol로 구성된 리스트는 모두 (cons 'symbol-1 (cons 'symbol-2 empty))
;; 이며, 모든 경우가 기호 리스트 집합에 속한다.

;; ex 9.2.3
;; boolean list
;; 1. empty : empty list
;; 2. (cons b lob) : b is a boolean and lob is a list of booleans.

;; ch 9.3

;; contains-doll? : list-of-symbols -> boolean
;; to determine whether the symbol 'doll occurs on a-list-of-symbols
(define (contains-doll? a-list-of-symbols)
  (cond
   [(empty? a-list-of-symbols) false]
   [else
    (cond
     [(symbol=? (first a-list-of-symbols) 'doll) true]
     [else (contains-doll? (rest a-list-of-symbols))])]))

(boolean=? (contains-doll? empty) false)
(boolean=? (contains-doll? (cons 'ball empty)) false)
(boolean=? (contains-doll? (cons 'doll empty)) true)

;; ex 9.3.1
'ex-9.3.1
(contains-doll? empty) ;; false
(contains-doll? (cons 'ball empty)) ;; false
(contains-doll? (cons 'arrow (cons 'doll empty))) ;; true
(contains-doll? (cons 'bow (cons 'arrow (cons 'ball empty)))) ;; false

;; ex 9.3.2
(define (contains-doll-2? a-list-of-symbols)
  (cond
   [(empty? a-list-of-symbols) false]
   [else (or
          (symbol=? (first a-list-of-symbols) 'doll)
          (contains-doll? (rest a-list-of-symbols)))]))
'ex-9.3.2-test
(contains-doll-2? empty) ;; false
(contains-doll-2? (cons 'ball empty)) ;; false
(contains-doll-2? (cons 'arrow (cons 'doll empty))) ;; true
(contains-doll-2? (cons 'bow (cons 'arrow (cons 'ball empty)))) ;; false

;; ex 9.3.3
;; contains?: symbol, list-of-symbol -> boolean
;; symbol이 list-of-symbol 리스트에 나타나는지 판단.
(define (contains? s los)
  (cond
    [(empty? los) false]
    [else (or
           (symbol=? (first los) s)
           (contains? s (rest los)))]))
'ex-9.3.3-test
(contains? 'doll empty) ;; false
(contains? 'doll (cons 'ball empty)) ;; false
(contains? 'doll (cons 'arrow (cons 'doll empty))) ;; true
(contains? 'doll (cons 'bow (cons 'arrow (cons 'ball empty)))) ;; false

;; sum : list-of-numbers -> number
(define (sum lon)
  (cond
    [(empty? lon) 0]
    [else (+ (first lon) (sum (rest lon)))]))

;; ex 9.5.1
'ex-9.5.1
(= 0 (sum empty))
(= 1.0 (sum (cons 1.00 empty)))
(= 20.86 (sum (cons 17.05 (cons 1.22 (cons 2.59 empty)))))
(= 3.81 (sum (cons 1.22 (cons 2.59 empty))))

;; ex 9.5.2
;; how-many-symbols : a-list-of-symbol -> number
(define (how-many-symbols los)
  (cond
    [(empty? los) 0]
    [else (+ 1 (how-many-symbols (rest los)))]))

;; how-many-numbers : a-list-of-number -> number
(define (how-many-numbers lon)
  (cond
    [(empty? lon) 0]
    [else (+ 1 (how-many-numbers (rest lon)))]))

'ex-9.5.2-test
(= 2 (how-many-symbols (cons 'a (cons 'b empty))))
(= 2 (how-many-numbers (cons 1 (cons 2 empty))))
;; 두 함수는 동일하다. symbol인지 number 인지 구분하지 않는다.

;; ex9.5.3
;; dollar-store? : a-list-of-number -> boolean
(define (dollar-store? prices)
  (cond
    [(empty? prices) true]
    [else
     (and (<= (first prices) 1)
          (dollar-store? (rest prices)))]))

'ex-9.5.3-test
(boolean=? (dollar-store? empty) true)
(boolean=? (not (dollar-store? (cons .75 (cons 1.95 (cons .25 empty))))) true)
(boolean=? (dollar-store? (cons .75 (cons .95 (cons .25 empty)))) true)

(define (general-dollar-store? prices threshold)
  (cond
    [(empty? prices) true]
    [else
     (and (<= (first prices) threshold)
          (general-dollar-store? (rest prices) threshold))]))

'ex-9.5.3-general-test
(boolean=? (general-dollar-store? empty 1) true)
(boolean=? (not (general-dollar-store? (cons .75 (cons 1.95 (cons .25 empty))) 1)) true)
(boolean=? (general-dollar-store? (cons .75 (cons .95 (cons .25 empty))) 1) true)


;; ex 9.5.4
;; check-range1 : list-of-number -> boolean
(define (check-range1 measurements)
  (cond
    [(empty? measurements) true]
    [else
     (and
      (and (> (first measurements) 5) (< (first measurements) 95))
      (check-range1 (rest measurements)))]))
'ex-9.5.4-check-range1-test
(boolean=? (check-range1 empty) true)
(boolean=? (check-range1 (cons 10 empty)) true)
(boolean=? (check-range1 (cons 10 (cons 100 empty))) false)

;; check-range는 있는 함수임.
(define (check-range-2 measurements low high)
  (cond
    [(empty? measurements) true]
    [else
     (and
      (and (> (first measurements) low) (< (first measurements) high))
      (check-range-2 (rest measurements) low high))]))

'ex-9.5.4-check-range-2-test
(boolean=? (check-range-2 empty 0 100) true)
(boolean=? (check-range-2 (cons 10 empty) 0 100) true)
(boolean=? (check-range-2 (cons 10 (cons 100 empty)) 30 80) false)

;; ex 9.5.5
;; convert : list-of-numbers -> number
(define (convert lon)
  (cond
    [(empty? lon) 0]
    [else
     (+ (first lon)
        (* 10 (convert (rest lon))))]))
'ex-9.5.5-convert-test
(= 4321 (convert (cons 1 (cons 2 (cons 3 (cons 4 empty))))))

;; check-guess-for-list : list-of-numbers, number -> symbol
;; return symbol is one of 'TooSmall, 'Perfect, 'TooLarge
(define (check-guess-for-list guesses target)
  (cond
    [(< (convert guesses) target) 'TooSmall]
    [(= (convert guesses) target) 'Perfect]
    [(> (convert guesses) target) 'TooLarge]))

;; ex 9.5.6
;; delta : list-of-number, list-of-number -> number
(define (delta beginning end)
  (- (sum end) (sum beginning)))

;; ex 9.5.7
;; average-price : list-of-number -> number
(define (average-price prices)
  (cond
    [(empty? prices) 0]
    [else (/ (sum prices) (how-many-numbers prices))]))

;; Iterative Refinement ???
;; ====

;; ex 9.5.8
;; draw-circles : posn, list-of-numbers -> boolean
(define (draw-circles p list-of-r)
  (cond
    [(empty? list-of-r) true]
    [else
     (and
      (draw-circle p (first list-of-r) 'blue)
      (draw-circles p (rest list-of-r)))]))

;; test:
;; (start 300 300)
;; (draw-circles (make-posn 150 150)
;;               (cons 10 (cons 20 (cons 30 (cons 40 (cons 50 (cons 60 empty)))))))

;; ex 11.3.2
(define (random-n-m n m)
  (+ (random (- m n)) n))
(define (tie-dyed n)
  (cond
    [(zero? n) empty]
    [else
     (cons (random-n-m 20 120)
           (tie-dyed (sub1 n)))]))
;(start 300 300)
;(draw-circles (make-posn 150 150)
;              (tie-dyed 30))

;; ex 11.3.3
(define (create-temps n low high)
  (cond
    [(zero? n) empty]
    [else
     (cons (random-n-m low high)
           (create-temps (sub1 n) low high))]))

(check-range-2 (create-temps 20 0 100) 5 95)
