;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ch25) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
;; 25.1
(define-struct ball (x y delta-x delta-y))

(define (draw-and-clear a-ball)
  (and (draw-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)
       (sleep-for-a-while DELAY)
       (clear-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)))

(define (move-ball a-ball)
  (make-ball (+ (ball-x a-ball) (ball-delta-x a-ball))
             (+ (ball-y a-ball) (ball-delta-y a-ball))
             (ball-delta-x a-ball)
             (ball-delta-y a-ball)))

(define WIDTH 100)
(define HEIGHT 100)
(define DELAY .1)

(define (out-of-bound? a-ball)
  (not
   (and
    (<= 0 (ball-x a-ball) WIDTH)
    (<= 0 (ball-y a-ball) HEIGHT))))

(check-expect (out-of-bound? (make-ball 3 3 10 10)) false)
(check-expect (out-of-bound? (make-ball 300 30 10 10)) true)
(check-expect (out-of-bound? (make-ball 30 300 10 10)) true)

(define (move-until-out a-ball)
  (cond
    [(out-of-bound? a-ball) true]
    [else (and
           (draw-and-clear a-ball)
           (move-until-out (move-ball a-ball)))]))

;; ex 25.1.1
;; infinite loop

;; ex 25.1.2
(define (move-balls ball-list)
  (let ((balls (filter
                (lambda (b) (not (out-of-bound? b)))
                ball-list)))
    (cond
      [(empty? balls) true]
      [else (and
             (draw-and-clear-balls balls)
             (move-balls (map move-ball balls)))])))

(define (draw-and-clear-balls ball-list)
  (and
   (andmap (lambda (a-ball) (draw-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)) ball-list)
   (sleep-for-a-while DELAY)
   (andmap (lambda (a-ball) (clear-solid-disk (make-posn (ball-x a-ball) (ball-y a-ball)) 5 'red)) ball-list)))

;; sample data
(define ball-data (list (make-ball 0 0 1 1)
                        (make-ball 0 0 1 0)
                        (make-ball 0 0 0 1)
                        (make-ball 50 50 -1 0)
                        (make-ball 50 50 0 -1)
                        (make-ball 50 50 1 0)
                        (make-ball 50 50 0 1)))

;; 25.2

;; from ch 12
;; sort : list-of-numbers -> list-of-numbers
;; to create a sorted list of numbers from all the numbers in alon
(define (legacy-sort alon)
  (cond
    [(empty? alon) empty]
    [else (insert (first alon) (legacy-sort (rest alon)))]))

;; examples & test
;(check-expect (legacy-sort empty) empty)
;(check-expect (legacy-sort (cons 1297.04 (cons 20000.00 (cons -505.25 empty)))) (cons 20000.00 (cons 1297.04 (cons -505.25 empty))))

;; insert : number list-of-numbers -> list-of-numbers
;; to create a list of numbers from n and the numbers on alon
;; that is sorted in descending order; alon is already sorted
(define (insert n alon)
  (cond
    [(empty? alon) (cons n empty)]
    [else
     (cond
       [(< n (first alon)) (cons n alon)]
       [else (cons (first alon) (insert n (rest alon)))])]))

;; examples & test
;(check-expect (insert 5 empty) (cons 5 empty))
;(check-expect (insert 1297.04 (cons 20000.00 (cons -505.25 empty))) (cons 20000.00 (cons 1297.04 (cons -505.25 empty))))

;; 25.2.1
(define nums (list 11 9 2 18 14 4 1))
;(list 9 2 4 1) 11 (list 18 14)
;((list 2 4 1) 9 empty) 11 ((list 14) 18 empty)
;(((list 1) 2 (list 4)) 9 empty) 11 ((empty 14 empty) 18 empty)
;(((empty 1 empty) 2 (empty 4 empty)) 9 empty) 11 ((empty 14 empty) 18 empty)
;(list 1 2 4 9 11 14 18)

(define (quick-sort alon)
  (cond
    [(empty? alon) empty]
    [else
     (append
      (quick-sort (smaller-items alon (first alon)))
      (list (first alon))
      (quick-sort (larger-items alon (first alon))))]))

(define (smaller-items alon pivot)
  (filter (lambda (v) (< v pivot)) alon))

(define (larger-items alon pivot)
  (filter (lambda (v) (> v pivot)) alon))

(check-expect (quick-sort nums) (list 1 2 4 9 11 14 18))

;; ex 25.2.2
;; 기존방식
(check-expect (quick-sort (list 11 8 14 7)) (list 7 8 11 14))
;1.
;(append (quick-sort (list 8 7))
;        (list 11)
;        (quick-sort (list 14)))
;2.
;(append (append (quick-sort (list 7))
;                (list 8)
;                (quick-sort empty))
;        (list 11)
;        (append (quick-sort empty)
;                (list 14)
;                (quick-sort empty)))
;3.
;(append (append (append (quick-sort empty)
;                        (list 7)
;                        (quick-sort empty))
;                (list 8)
;                empty)
;        (list 11)
;        (append empty
;                (list 14)
;                empty))
;4.
;(append (append (append empty
;                        (list 7)
;                        empty)
;                (list 8)
;                empty)
;        (list 11)
;        (append empty
;                (list 14)
;                empty))

(define (quick-sort-2 alon)
  (cond
    [(empty? alon) empty]
    [(empty? (rest alon)) alon]
    [else
     (append
      (quick-sort-2 (smaller-items alon (first alon)))
      (list (first alon))
      (quick-sort-2 (larger-items alon (first alon))))]))


;; 하나의 항목만 갖는 리스트를 처리한 경우
(check-expect (quick-sort-2 (list 11 8 14 7)) (list 7 8 11 14))
;1.
;(append (quick-sort-2 (list 8 7))
;        (list 11)
;        (quick-sort-2 (list 14)))
;2.
;(append (append (quick-sort-2 (list 7))
;                (list 8)
;                (quick-sort-2 empty))
;        (list 11)
;        (list 14))
;3.
;(append (append (list 7)
;                (list 8)
;                empty)
;        (list 11)
;        (list 14))

;; ex 25.2.3
(define BASE_LENGTH 4)
(define (quick-sort-3 alon)
  (cond
    [(empty? alon) empty]
    [(empty? (rest alon)) alon]
    [(< (length alon) BASE_LENGTH) (legacy-sort alon)]
    [else
     (append
      (quick-sort-3 (smaller-items alon (first alon)))
      (list (first alon))
      (quick-sort-3 (larger-items alon (first alon))))]))

;; test
(check-expect (quick-sort-3 (list 11 8 14 7)) (list 7 8 11 14))

;; ex 25.2.4
(define (qs alon)
  (cond
    [(empty? alon) empty]
    [(empty? (rest alon)) alon]
    [else
     (append
      (qs (smallers (rest alon) (first alon)))
      (list (first alon))
      (qs (largers (rest alon) (first alon))))]))

(define (smallers alon pivot)
  (filter (lambda (v) (<= v pivot)) alon))

(define (largers alon pivot)
  (filter (lambda (v) (> v pivot)) alon))

(check-expect (qs (list 11 8 14 7 11 3 9 8 7)) (list 3 7 7 8 8 9 11 11 14))

;; ex 25.2.5
;; ok

;; ex 25.2.6
(define (general-qs pred? alon)
  (cond
    [(empty? alon) empty]
    [(empty? (rest alon)) alon]
    [else
     (append
      (general-qs pred? (filter (lambda (x) (pred? x (first alon))) (rest alon)))
      (list (first alon))
      (general-qs pred? (filter (lambda (x) (not (pred? x (first alon)))) (rest alon))))]))

(check-expect (general-qs < (list 11 8 14 7 11 3 9 8 7)) (list 3 7 7 8 8 9 11 11 14))
(check-expect (general-qs > (list 11 8 14 7 11 3 9 8 7)) (list 14 11 11 9 8 8 7 7 3))
(check-expect (general-qs <= (list 11 8 14 7 11 3 9 8 7)) (list 3 7 7 8 8 9 11 11 14))
(check-expect (general-qs >= (list 11 8 14 7 11 3 9 8 7)) (list 14 11 11 9 8 8 7 7 3))
