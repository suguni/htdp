;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch9) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
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

