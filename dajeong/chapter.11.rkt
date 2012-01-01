;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter.11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;hellos : N -> list-of-symbols
;;n개의 'hello가 들어있는 리스트를 생성한다.
(define (hellos n)
 (cond
   [(zero? n) empty]
   [else (cons 'hello (hellos (sub1 n)))]))

;;ex.11.2.1
(define (repeat n symbol)
  (cond
    [(zero? n) empty]
    [else (cons symbol (repeat (sub1 n) symbol))]))

;;ex.11.2.2
(define (f x)
  (+ (* 3 (* x x))
     (+ (* -6 x)
        -1)))
(define (tabulate-f n)
  (cond
    [(zero? n) empty]
    [else (cons (make-posn n (f n)) (tabulate-f (sub1 n)))]))

;;ex.11.2.4
(define (depth deep-list)
  (cond
    [(empty? deep-list) 0]
    [else (add1 (depth (first deep-list)))]))

(define (make-deep s n)
  (cond
    [(zero? n) s]
    [else (cons (make-deep s (sub1 n)) empty)]))

;; ex.11.3.1
(define (random-n-m n m)
  (+ (random (- m n)) n))

;; ex.11.3.2

(define (tie-dyed n)
  (cond
    [(zero? n) empty]
    [else (cons (random-n-m 20 120)
                (tie-dyed (sub1 n)))]))

;;ex.11.3.3
(define (create-temp low high n)
  (cond
    [(zero? n) empty]
    [else (cons (random-n-m low high) (create-temp low high (sub1 n)))]))

;;11.4 자연수에 대한 또 다른 데이터 정의
(define (! n)
  (cond 
    [(zero? n) 1]
    [else (* n (! (sub1 n)))]))
(check-expect 2 (! 2))

;;ex.11.4.1
;(define (product-from-20 n)
;  (/ (! n) (! 20)))
 
;;ex.11.4.2
(define (product n m)
  (/ (! m) (! n)))

;ex.11.4.3
(define (product-from-minus-11 n)
  (cond
    [(= n -11) 1]
    [else (* n (product-from-minus-11 (sub1 n)))]))
;;(product-from-minus-11 -10)

;;ex.11.4.4
(define (tabulate-f20 n)
  (cond
    [(<= n 20) empty]
    [else (cons (make-posn n (f n)) (tabulate-f20 (sub1 n)))]))

;;ex.11.4.5
(define (tabulate-f-lim limit n)
  (cond
    [(= limit n) empty]
    [else (cons (make-posn n (f n)) (tabulate-f-lim limit (sub1 n)))]))

;;ex.11.4.6
(define (tabulate-f-up-to-20 n)
  (cond
    [(= n 0) empty]
    [else (cons (make-posn n (f n)) (tabulate-f-up-to-20 (sub1 n)))]))

;;ex.11.4.7
