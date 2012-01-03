;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define (hellos n)
  (cond
    [(zero? n) empty]
    [else (cons 'hello (hellos (sub1 n)))]))

;; ex 11.2.1
;; repeat : number symbol -> list-of-symbols
(define (repeat n s)
  (cond
    [(zero? n) empty]
    [else (cons s (repeat (sub1 n) s))]))

(check-expect (repeat 3 'god)
              (cons 'god (cons 'god (cons 'god empty))))

;; ex 11.2.2
;; f : number  ->  number
(define (f x)
  (+ (* 3 (* x x)) 
     (+ (* -6 x)
        -1)))

;; tabulate-f : number -> list-of-posn
(define (tabulate-f n)
  (cond
    [(zero? n) empty]
    [else (cons (make-posn n (f n)) (tabulate-f (sub1 n)))]))

(check-expect (tabulate-f 4)
              (cons
               (make-posn 4 23)
               (cons (make-posn 3 8) (cons (make-posn 2 -1) (cons (make-posn 1 -4) empty)))))

;; ex 11.2.3
;; 10.3.1, 10.3.6 참조 필요

;; ex 11.2.4
;; depth : deep-list -> number
(define (depth deep-list) 
  (cond 
    [(symbol? deep-list) 0]
    [else (add1 (depth (first deep-list)))]))

(check-expect (depth (cons (cons (cons 'e empty) empty) empty)) 3)

;; make-deep : symbol number -> deep-list
(define (make-deep s n)
  (cond 
    [(zero? n) s]
    [else (cons (make-deep s (sub1 n)) empty)]))

(check-expect (make-deep 's 4)
              (cons (cons (cons (cons 's empty) empty) empty) empty))
(check-expect (depth (make-deep 's 20)) 20)

;; ex 11.3.1
;; random-n-m : integer integer  ->  integer
;; (정답) n 이상 m 미만 사이의 임의 자연수를 출력한다.
;; n < m이라 가정
(define (random-n-m n m)
  (+ (random (- m n)) n))

;; ex 11.3.2
;; tie-dyed : number -> number
(define (tie-dyed n)
  (cond
    [(zero? n) empty]
    [else (cons (random-n-m 20 120)
                (tie-dyed (sub1 n)))]))

;; ex 11.3.3
;; create-temps : number number number -> list-of-numbers
(define (create-temps n low high)
  (cond
    [(zero? n) empty]
    [else (cons (random-n-m low high)
                (create-temps (sub1 n) low high))]))
;; 정확히 매칭되는 결과를 기대하긴 힘들지만, 해당 범위(low ~ high)라는 전제 조건이 존재하므로 어느 정도 check-range를 반복하면 결과에 대한 기대치를 가질 수 있겠다.

;; ex 11.3.4
;; create-prices : number -> list-of-numbers
(define (create-prices n)
  (cond
    [(zero? n) empty]
    [else (cons (/ (+ (random 100) 1) 10)
                (create-prices (sub1 n)))]))

;; ex 11.3.5
;; pass


;; ch 11.4 --

;; ! : N -> N
(define (! n)
  (cond
    [(zero? n) 1]
    [else (* n (! (sub1 n)))]))

;; ex 11.4.1
;(! 2)
;(* 2 (! (sub1 2)))
;(* 2 (* 1 (! sub1 1)))
;(* 2 (* 1 1))
;(* 2 1)
;2

(check-expect (! 10) 3628800)
(check-expect (! 100) 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000)

;; product-from-20 : N [>= 20] -> N
(define (product-from-20 n-above-20)
  (cond
    [(= n-above-20 20) 1]
    [else (* n-above-20 (product-from-20 (sub1 n-above-20)))]))

;; product : N [limit] N [>= limit] -> N
(define (product limit n)
  (cond
    [(= n limit) 1]
    [else (* n (product limit (sub1 n)))]))


;; ex 11.4.3
;; product-from-minus-11 : N [>= -11] -> N
(define (product-from-minus-11 n-above-minus-11)
  (cond
    [(= n-above-minus-11 -11) 1]
    [else (* n-above-minus-11 (product-from-minus-11 (sub1 n-above-minus-11)))]))

;; ex 11.4.4
;; tabulate-f20 : N [>= 20] -> list-of-posn
(define (tabulate-f20 n)
  (cond
    [(= n 20) empty]
    [else (cons (make-posn n (f n)) (tabulate-f20 (sub1 n)))]))

(check-expect (tabulate-f20 23) 
              (cons (make-posn 23 1448) (cons (make-posn 22 1319) (cons (make-posn 21 1196) empty))))

;; ex 11.4.5
;; tabulate-f-lim : N [>= limit] N [limit] -> list-of-posn
(define (tabulate-f-lim n limit)
  (cond
    [(= n limit) empty]
    [else (cons (make-posn n (f n)) (tabulate-f-lim (sub1 n) limit))]))

(check-expect (tabulate-f-lim 15 10)
              (cons
               (make-posn 15 584)
               (cons
                (make-posn 14 503)
                (cons (make-posn 13 428) (cons (make-posn 12 359) (cons (make-posn 11 296) empty))))))

;; ex 11.4.6
;; tabulate-f-up-to-20 : N [<= 20] -> list-of-posn
(define (tabulate-f-up-to-20 n-below-20)
  (cond
    [(= n-below-20 20) empty]
    [else (cons (make-posn n-below-20 (f n-below-20)) 
                (tabulate-f-up-to-20 (add1 n-below-20)))]))

;; ex 11.4.7
;; is-not-divisible-by<=i : N [>= 1] N [> i] -> boolean
(define (is-not-divisible-by<=i i m)
  (cond
    [(= i 1) true]
    [(= (remainder m i) 0) false]
    [else (is-not-divisible-by<=i (sub1 i) m)]))

(check-expect (is-not-divisible-by<=i 4 5) true)
(check-expect (is-not-divisible-by<=i 4 8) false)

;; prime? : N -> boolean
(define (prime? n)
  (is-not-divisible-by<=i (ceiling (/ n 2)) n))

(check-expect (prime? 4) false)
(check-expect (prime? 121) false)
(check-expect (prime? 191) true)

;; ch 11.5 --

;; add-to-pi : N -> number
(define (add-to-pi n)
  (cond
    [(zero? n) 3.14]
    [else (add1 (add-to-pi (sub1 n)))]))

;; ex 11.5.1
;; add : N N -> number
(define (add n x)
  (cond
    [(zero? n) x]
    [else (add1 (add (sub1 n) x))]))

;; ex 11.5.2
;; multiply-by-pi (skip)
;; multiply : N N -> number 
(define (multiply n x)
  (cond
    [(zero? n) 0]
    [else (add x (multiply (sub1 n) x))]))

;; ex 11.5.3
;; exponent : N N -> number
(define (exponent n x)
  (cond
    [(zero? n) 1]
    [else (multiply x (exponent (sub1 n) x))]))
