;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch19) (read-case-sensitive #t) (teachpacks ((lib "dir.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.ss" "teachpack" "htdp")))))
;; ch 19. 정의 사이의 연관성

(define (filter1 rel-op lon t)
  (cond
    [(empty? lon) empty]
    [else
     (cond
       [(rel-op (first lon) t) (cons (first lon) 
                                     (filter1 rel-op (rest lon) t))]
       [else (filter1 rel-op (rest lon) t)])]))

(check-expect (filter1 < empty 5) empty)
(check-expect (filter1 < (cons 4 empty) 5)
              (cons 4 empty))

;; ex 19.1.1
;(filter1 < (cons 6 (cons 4 empty)) 5)
;
;(cond
;  [(empty? (cons 6 (cons 4 empty))) empty]
;  [else
;   (cond
;     [(< (first (cons 6 (cons 4 empty))) 5) (cons (first (cons 6 (cons 4 empty))) 
;                                                  (filter1 < (rest (cons 6 (cons 4 empty))) 5))]
;     [else (filter1 < (rest (cons 6 (cons 4 empty))) 5)])])
;
;(cond
;  [false empty]
;  [else
;   (cond
;     [(< (first (cons 6 (cons 4 empty))) 5) (cons (first (cons 6 (cons 4 empty))) 
;                                                  (filter1 < (rest (cons 6 (cons 4 empty))) 5))]
;     [else (filter1 < (rest (cons 6 (cons 4 empty))) 5)])])
;
;(cond
;  [(< (first (cons 6 (cons 4 empty))) 5) (cons (first (cons 6 (cons 4 empty))) 
;                                               (filter1 < (rest (cons 6 (cons 4 empty))) 5))]
;  [else (filter1 < (rest (cons 6 (cons 4 empty))) 5)])
;
;(cond
;  [(< 6 5) (cons (first (cons 6 (cons 4 empty))) 
;                 (filter1 < (rest (cons 6 (cons 4 empty))) 5))]
;  [else (filter1 < (rest (cons 6 (cons 4 empty))) 5)])
;
;(cond
;  [false (cons (first (cons 6 (cons 4 empty))) 
;                 (filter1 < (rest (cons 6 (cons 4 empty))) 5))]
;  [else (filter1 < (rest (cons 6 (cons 4 empty))) 5)])
;
;(filter1 < (rest (cons 6 (cons 4 empty))) 5)
;
;(filter1 < (cons 4 empty) 5)

;; ex 19.1.2
;(filter1 > (cons 8 (cons 6 (cons 4 empty))) 5)
;(cons 8 (filter1 > (cons 6 (cons 4 empty)) 5))
;(cons 8 (cons 6 (filter1 > (cons 4 empty) 5)))
;(cons 8 (cons 6 (filter1 > empty 5)))
;(cons 8 (cons 6 empty))

(define (squared>? x c)
  (> (* x x) c))

;; ex 19.1.3
;(filter1 squared>? (list 4 5) 10)
;
;(cond
;  [(empty? (list 4 5)) empty]
;  [else
;   (cond
;     [(squared>? (first (list 4 5)) 10) (cons (first (list 4 5)) 
;                                              (filter1 squared>? (rest (list 4 5)) 10))]
;     [else (filter1 squared>? (rest (list 4 5)) 10)])])
;
;(cond
;  [false empty]
;  [else
;   (cond
;     [(squared>? (first (list 4 5)) 10) (cons (first (list 4 5)) 
;                                              (filter1 squared>? (rest (list 4 5)) 10))]
;     [else (filter1 squared>? (rest (list 4 5)) 10)])])
;
;(cond
;  [(squared>? (first (list 4 5)) 10) (cons (first (list 4 5)) 
;                                           (filter1 squared>? (rest (list 4 5)) 10))]
;  [else (filter1 squared>? (rest (list 4 5)) 10)])
;
;(cond
;  [(squared>? 4 10) (cons (first (list 4 5)) 
;                          (filter1 squared>? (rest (list 4 5)) 10))]
;  [else (filter1 squared>? (rest (list 4 5)) 10)])
;
;(cond
;  [true (cons (first (list 4 5)) 
;              (filter1 squared>? (rest (list 4 5)) 10))]
;  [else (filter1 squared>? (rest (list 4 5)) 10)])
;
;(cons (first (list 4 5))
;      (filter1 squared>? (rest (list 4 5)) 10))
;
;(cons 4 (filter squared>? (list 5) 10))

;; 19.1.4
;; squared10? : number number -> boolean
(define (squared10? x c)
  (> (sqr x) 10))

;; below : lon number -> lon
(define (below lon t)
  (local ((define (pred? a)
            (< a t)))
    (filter pred? lon)))

(check-expect (below (list 1 2 3 4 5 6) 3)
              (list 1 2))

;; above : lon number -> lon
(define (above lon t)
  (local ((define (pred? a)
            (> a t)))
    (filter pred? lon)))

(check-expect (above (list 1 2 3 4 5 6) 3)
              (list 4 5 6))


;; ex 19.1.5
;; mini : nelon -> number
;; alon에서 가장 작은 수를 찾는다.
(define (mini alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else 
     (cond
       [(< (first alon) (mini (rest alon))) (first alon)]
       [else (mini (rest alon))])]))

;; maxi : nelon -> number
;; alon에서 가장 큰 수를 찾는다.
(define (maxi alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else 
     (cond
       [(> (first alon) (maxi (rest alon))) (first alon)]
       [else (maxi (rest alon))])]))

;; min-max : nelon -> number
(define (min-max op alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else 
     (cond
       [(op (first alon) (min-max op (rest alon))) (first alon)]
       [else (min-max op (rest alon))])]))

;; min1 : nelon -> number
(define (min1 alon)
  (min-max < alon))

;; max1 : nelon -> number
(define (max1 alon)
  (min-max > alon))

(define mm1 (list 3 7 6 2 9 8))
(define mm2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(define mm3 (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))

(check-expect (min1 mm1) 2)
(check-expect (min1 mm2) 1)
(check-expect (min1 mm3) 1)
(check-expect (max1 mm1) 9)
(check-expect (max1 mm2) 20)
(check-expect (max1 mm3) 20)

;; (min1 mm3)와 (max1 mm2)가 느리다.

;; min-max2 : nelon -> number
(define (min-max2 op alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else (local ((define first-item (first alon))
                  (define rest-filtered (min-max2 op (rest alon)))
                  (define (sel a b)
                    (cond
                      [(op a b) a]
                      [else b])))
            (sel first-item rest-filtered))]))

;; min2 : nelon -> number
(define (min2 alon) (min-max2 < alon))
;; max2 : nelon -> number
(define (max2 alon) (min-max2 > alon))

(check-expect (min2 mm1) 2)
(check-expect (min2 mm2) 1)
(check-expect (min2 mm3) 1)
(check-expect (max2 mm1) 9)
(check-expect (max2 mm2) 20)
(check-expect (max2 mm3) 20)
;; (min2 mm3)
;; (max2 mm2)
;; 빨라진거 같다.

;; ex 19.1.6
;; sortm : lon -> lon
(define (sortm alon)
  (local ((define (sort alon)
            (cond
              [(empty? alon) empty]
              [else (insert (first alon) (sort (rest alon)))]))
          (define (insert an alon)
            (cond
              [(empty? alon) (list an)]
              [else (cond
                      [(> an (first alon)) (cons an alon)]
                      [else (cons (first alon)
                                  (insert an (rest alon)))])])))
    (sort alon)))

(check-expect (sortm (list 2 3 1 5 4)) (list 5 4 3 2 1))

;; sort-s : lon op -> lon
(define (sort-s alon op)
  (local ((define (sort alon)
            (cond
              [(empty? alon) empty]
              [else (insert (first alon) (sort (rest alon)))]))
          (define (insert an alon)
            (cond
              [(empty? alon) (list an)]
              [else (cond
                      [(op an (first alon)) (cons an alon)]
                      [else (cons (first alon)
                                  (insert an (rest alon)))])])))
    (sort alon)))

(define (sort-asc lon) (sort-s lon <))
(define (sort-des lon) (sort-s lon >))

(check-expect (sort-asc (list 2 3 1 5 4)) (list 1 2 3 4 5))
(check-expect (sort-des (list 2 3 1 5 4)) (list 5 4 3 2 1))

;; 19.2

(define-struct ir (name price))

(define (below-ir1 aloir t)
  (filter1 <ir aloir t))

(define (<ir ir p)
  (< (ir-price ir) p))

(define (find aloir t)
  (cons? (filter1 eq-ir? aloir t)))

(define (eq-ir? ir p)
  (symbol=? (ir-name ir) p))

;; ex 19.2.1
(check-expect (below-ir1 (list (make-ir 'doll 8) (make-ir 'robot 12)) 10) (list (make-ir 'doll 8)))
;(below-ir1 (list (make-ir 'doll 8) (make-ir 'robot 12)) 10)
;(filter1 <ir (list (make-ir 'doll 8) (make-ir 'robot 12)) 10)
(check-expect (find (list (make-ir 'doll 8) (make-ir 'robot 12) (make-ir 'doll 13)) 'doll) true)
;(find (list (make-ir 'doll 8) (make-ir 'robot 12) (make-ir 'doll 13)) 'doll)
;(cons? (filter1 eq-ir? (list (make-ir 'doll 8) (make-ir 'robot 12) (make-ir 'doll 13)) 'doll))

;; ex 19.2.2
;; alon은 재고 리스트이고 op는 두 재고의 이름을 비교하는 함수를 작성하면 된다.

;; ascending sort op
(define (ir-sort-asc-op ir1 ir2)
  (local ((define n1 (symbol->string (ir-name ir1)))
          (define n2 (symbol->string (ir-name ir2))))
    (string<? n1 n2)))

;; descending sort op
(define (ir-sort-des-op ir1 ir2)
  (local ((define n1 (symbol->string (ir-name ir1)))
          (define n2 (symbol->string (ir-name ir2))))
    (string>? n1 n2)))

;; ex 19.2.3
(define-struct pair (left right))
;; 1. (listof (pair number number))
;; (list (make-pair 1 2) (make-pair 3 4) (make-pair 5 6))
;; 2. (listof (pair symbol number))
;; (list (make-pair 'a 1) (make-pair 'b 2) (make-pair 'c 3))
;; 3. (listof (pair symbol symbol))
;; (list (make-pair 'a 'x) (make-pair 'b 'y) (make-pair 'c 'z))

;; lefts : (listof (pair X Y)) -> (listof X)
(define (lefts lop)
  (cond
    [(empty? lop) empty]
    [else
     (cons (pair-left (first lop))
           (lefts (rest lop)))]))

;; tests
(check-expect (lefts (list (make-pair 1 2) (make-pair 3 4) (make-pair 5 6)))
              (list 1 3 5))
(check-expect (lefts (list (make-pair 'a 1) (make-pair 'b 2) (make-pair 'c 3)))
              (list 'a 'b 'c))
(check-expect (lefts (list (make-pair 'x 'a) (make-pair 'y 'b) (make-pair 'z 'c)))
              (list 'x 'y 'z))

;; ex 19.2.4
;; last : (non-empty-listof ITEM) -> ITEM
(define (last seq)
  (cond
    [(empty? (rest seq)) (first seq)]
    [else (last (rest seq))]))
;; tests
(check-expect (last (list 1 2 3 4 5)) 5)
