;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch9) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp")))))

;; ex 9.1.4
#| 정의:
list-of-2-symbol은 다음과 같다.
  (cons a (cons b empty))
여기에서 a, b는 기호다.
|#
  
;; contains-2-doll? : list-of-2-symbol -> boolean
(define (contains-2-doll? a-list-of-2-symbol)
  (cond
    [(equal? (first a-list-of-2-symbol) 'doll) true]
    [(equal? (first (rest a-list-of-2-symbol)) 'doll) true]
    [else false]))

(check-expect (contains-2-doll? (cons 'dog (cons 'doll empty))) true)
(check-expect (contains-2-doll? (cons 'dog (cons 'cat empty))) false)
(check-expect (contains-2-doll? (cons 'doll (cons 'cat empty))) true)

;; ex 9.2.1
#|
재고 목록:
(cons 'bow (cons 'arrow empty))

(cons 'arrow empty)
'arrow는 기호이고, empty는 기호 리스트이기 때문에 위 항목은 기호 리스트 이다.
(cons 'bow (cons 'arrow empty))
'bow는 기호이고, (cons 'arrow empty)는 위에서 증명하였든 기호 리스트이기 때문에 제시 된 재고목록은 기호 리스트 이다.
|#

;; ex 9.2.2
;; 두 개의 기호로 구성 된 모든 리스트에서, 리스트 내부를 구성하는 데이터 종류는 기호만 있으므로 이는 기호 리스트 집합에 속한다.

;; ex 9.2.3
#|
불린 리스트(list-of-boolean)는 다음 두 가지 중 하나다.
  1. empty : 비어 있는 리스트
  2. (cons b lob) : s는 불린이고 los는 불린 리스트
|#

;; ex 9.3.1
(define (contains-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (cond
            [(symbol=? (first a-list-of-symbols) 'doll) true]
            [else (contains-doll? (rest a-list-of-symbols))])]))

(check-expect (contains-doll? empty) false)
(check-expect (contains-doll? (cons 'ball empty)) false)
(check-expect (contains-doll? (cons 'arrow (cons 'doll empty))) true)
(check-expect (contains-doll? (cons 'bow (cons 'arrow (cons 'ball empty)))) false)

;; ex 9.3.2
(define (other-contains-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (or (symbol=? (first a-list-of-symbols) 'doll)
              (other-contains-doll? (rest a-list-of-symbols)))]))

(check-expect (other-contains-doll? empty) false)
(check-expect (other-contains-doll? (cons 'ball empty)) false)
(check-expect (other-contains-doll? (cons 'arrow (cons 'doll empty))) true)
(check-expect (other-contains-doll? (cons 'bow (cons 'arrow (cons 'ball empty)))) false)

;; ex 9.3.3
;; contains? : symbol list-of-symbols -> boolean
(define (contains? target a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (or (symbol=? (first a-list-of-symbols) target)
              (contains? target (rest a-list-of-symbols)))]))

(check-expect (contains? 'foo empty) false)
(check-expect (contains? 'foo (cons 'bar empty)) false)
(check-expect (contains? 'foo (cons 'bar (cons 'foo empty))) true)
(check-expect (contains? 'foo (cons 'bar (cons 'ball (cons 'fire empty)))) false)

;; ch 9.5
;; sum : list-of-numbers  ->  number
;; to compute the sum of the numbers on a-list-of-nums
;; (define (sum a-list-of-nums) ...)

(define (sum a-list-of-nums)
  (cond
    [(empty? a-list-of-nums) 0]
    [else (+ (first a-list-of-nums) (sum (rest a-list-of-nums)))]))


;; ex 9.5.1
(check-expect (sum empty) 0)
(check-expect (sum (cons 1.00 empty)) 1.0)
(check-expect (sum (cons 17.05 (cons 1.22 (cons 2.59 empty)))) 20.86)

;; ex 9.5.2
;; how-many-symbols : list-of-symbols  ->  number
(define (how-many-symbols a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) 0]
    [else (+ (how-many-symbols (rest a-list-of-symbols)) 1)]))

;; how-many-numbers : list-of-numbers  ->  number
(define (how-many-numbers a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) 0]
    [else (+ (how-many-numbers (rest a-list-of-numbers)) 1)]))

;; 리스트 데이터 입력을 보장한다는 가정이 있다면 별다른 차이가 없다.

;; ex 9.5.3
(define (dollar-store? a-list-of-nums)
  (cond
    [(empty? a-list-of-nums) true]
    [else (and (<= (first a-list-of-nums) 1)
               (dollar-store? (rest a-list-of-nums)))]))

(check-expect (dollar-store? empty) true)
(check-expect (not (dollar-store? (cons .75 (cons 1.95 (cons .25 empty))))) true)
(check-expect (dollar-store? (cons .15 (cons .05 (cons .25 empty)))) true)
;; 함수 일반화는 생략

;; ex 9.5.4
(define (check-temps-range a-list-of-temps min-temp max-temp)
  (cond
    [(empty? a-list-of-temps) true]
    [else (and 
           (and (>= (first a-list-of-temps) min-temp) 
                (<= (first a-list-of-temps) max-temp))
           (check-temps-range (rest a-list-of-temps) min-temp max-temp))]))

(check-expect (check-temps-range empty 5 95) true)
(check-expect (check-temps-range (cons 48 empty) 5 95) true)
(check-expect (check-temps-range (cons 48 empty) 90 95) false)
(check-expect (check-temps-range (cons 48 (cons 91 (cons 5 empty))) 5 95) true)

;; ex 9.5.5
(define (convert a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) 0]
    [else (+ (first a-list-of-numbers)
             (* (convert (rest a-list-of-numbers)) 10))]))

(check-expect (convert (cons 1 (cons 2 (cons 3 empty)))) 321)

(define (check-guess-for-list a-list-of-guesses target)
  (cond
    [(= (convert a-list-of-guesses) target) 'Perfect]
    [(> (convert a-list-of-guesses) target) 'TooLarge]
    [else 'TooSmall]))

;(guess-with-gui-list 5 check-guess-for-list)

;; ex 9.5.6
;; 리스트 병행 호출??

;; ex 9.5.7
(define (average-price a-list-of-prices)
  (cond
    [(empty? a-list-of-prices) (error 'average-price "list expects not empty")]
    [else (/ (sum a-list-of-prices) (how-many-numbers a-list-of-prices))]))

(check-expect (average-price (cons 95 (cons 105 empty))) 100)
(check-error (average-price empty))

;; ex 9.5.8
(define (draw-circles p a-list-of-radiuses)
  (cond
    [(empty? a-list-of-radiuses) true]
    [else (and (draw-circle p (first a-list-of-radiuses) 'red)
               (draw-circles p (rest a-list-of-radiuses)))]))

;(start 300 300)
;(draw-circles (make-posn 150 150) 
;              (cons 10 (cons 30 (cons 40 (cons 80 (cons 100 empty))))))