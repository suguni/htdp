;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ch 19.1

;; Figure 52:  Two similar functions

;; contains-doll? : los  ->  boolean
;; alos에 기호 'doll이 존재하는지 검사한다.
(define (contains-doll? alos)
  (cond
    [(empty? alos) false]
    [else
     (cond
       [(symbol=? (first alos) 'doll)
        true]
       [else 
        (contains-doll? (rest alos))])]))

;; contains-car? : los  ->  boolean
;; alos에 기호 'car이 존재하는지 검사한다.
(define (contains-car? alos)
  (cond
    [(empty? alos) false]
    [else
     (cond
       [(symbol=? (first alos) 'car)
        true]
       [else 
        (contains-car? (rest alos))])]))


;; contains? : symbol los  ->  boolean
;; alos에 기호 s가 존재하는지 검사한다.
(define (contains? s alos)
  (cond
    [(empty? alos) false]
    [else (cond
            [(symbol=? (first alos) s)
             true]
            [else 
             (contains? s (rest alos))])]))


;; Figure 53:  Two more similar functions

;; below : lon number  ->  lon
;; alon에서 t보다 작은 수들로 이루어진 리스트를 만든다.
(define (below alon t)
  (cond
    [(empty? alon) empty]
    [else
     (cond
       [(< (first alon) t)
        (cons (first alon)
              (below (rest alon) t))]
       [else
        (below (rest alon) t)])]))

;; above : lon number  ->  lon
;; alon에서 t보다 큰 수들로 이루어진 리스트를 만든다.
(define (above alon t)
  (cond
    [(empty? alon) empty]
    [else
     (cond
       [(> (first alon) t) 
        (cons (first alon)
              (above (rest alon) t))]
       [else
        (above (rest alon) t)])]))


(define (filter1 rel-op alon t)
  (cond
    [(empty? alon) empty]
    [else (cond
            [(rel-op (first alon) t) 
             (cons (first alon)
                   (filter1 rel-op (rest alon) t))]
            [else
             (filter1 rel-op (rest alon) t)])]))

(check-expect (filter1 < empty 5) empty)
(check-expect (filter1 < (cons 4 empty) 5) (cons 4 empty))
(check-expect (filter1 < (cons 6 (cons 4 empty)) 5) (cons 4 empty))


;; ex 19.1.1
;   (filter1 < (cons 6 (cons 4 empty)) 5)
; = (filter1 < (cons 4 empty) 5)

;(cond
; ((empty? (list 6 4)) empty)
; (else
;  (cond
;   ((< (first (list 6 4)) 5)
;    (cons
;     (first (list 6 4))
;     (filter1 < (rest (list 6 4)) 5)))
;   (else (filter1 < (rest (list 6 4)) 5)))))
;
;(cond
; (false empty)
; (else
;  (cond
;   ((< (first (list 6 4)) 5)
;    (cons (first (list 6 4)) (filter1 < (rest (list 6 4)) 5)))
;   (else (filter1 < (rest (list 6 4)) 5)))))
;
;(cond
; (else
;  (cond
;   ((< (first (list 6 4)) 5)
;    (cons (first (list 6 4)) (filter1 < (rest (list 6 4)) 5)))
;   (else (filter1 < (rest (list 6 4)) 5)))))
;
;(cond
; ((< (first (list 6 4)) 5)
;  (cons (first (list 6 4)) (filter1 < (rest (list 6 4)) 5)))
; (else (filter1 < (rest (list 6 4)) 5)))
;
;(cond
; ((< 6 5)
;  (cons (first (list 6 4)) (filter1 < (rest (list 6 4)) 5)))
; (else (filter1 < (rest (list 6 4)) 5)))
;
;(cond
; (false
;  (cons (first (list 6 4)) (filter1 < (rest (list 6 4)) 5)))
; (else (filter1 < (rest (list 6 4)) 5)))
;
;(cond (else (filter1 < (rest (list 6 4)) 5)))
;
;(filter1 < (rest (list 6 4)) 5)
;
;(filter1 < (list 4) 5)


;; ex 19.1.2
; 동일 형식



;; below1 : lon number  ->  lon
(define (below1 alon t)
  (filter1 < alon t))

;; above1 : lon number  ->  lon
(define (above1 alon t) 
  (filter1 > alon t))

;; squared>? : number number  ->  boolean
(define (squared>? x c)
  (> (* x x) c))

(check-expect (filter1 squared>? (list 1 2 3 4 5) 10) (list 4 5))


;; ex 19.1.3
; 동일 형식


;; ex 19.1.4
(define (filter2 predicate alon)
  (cond
    [(empty? alon) empty]
    [else (cond
            [(predicate (first alon)) 
             (cons (first alon)
                   (filter2 predicate (rest alon)))]
            [else
             (filter2 predicate (rest alon))])]))

(define (below2 alon t)
  (local ((define (x<? x)
            (< x t)))
    (filter2 x<? alon)))

(define (above2 alon t)
  (local ((define (x>? x)
            (> x t)))
    (filter2 x>? alon)))

(check-expect (below2 (list 1 2 3 4) 3) (list 1 2))
(check-expect (above2 (list 1 2 3 4) 3) (list 4))


;; Figure 54:  Two modifications of filter1

(define (filter1-mod1 rel-op alon t)
  (cond
    [(empty? alon) empty]
    [(rel-op (first alon) t) 
     (cons (first alon)
           (filter1-mod1 rel-op (rest alon) t))]
    [else
     (filter1-mod1 rel-op (rest alon) t)]))

(define (filter1-mod2 rel-op alon t)
  (cond
    [(empty? alon) empty]
    [else
     (local ((define first-item (first alon))
             (define rest-filtered
               (filter1-mod2 rel-op (rest alon) t)))
       (cond
         [(rel-op first-item t) 
          (cons first-item rest-filtered)]
         [else
          rest-filtered]))]))


;; ex 19.1.5
;; mini : nelon  ->  number
;; alon에서 가장 작은 수를 찾는다.
(define (mini alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else (cond
            [(< (first alon) 
                (mini (rest alon)))
             (first alon)]
            [else
             (mini (rest alon))])]))

;; maxi : nelon  ->  number
;; alon에서 가장 큰 수를 찾는다.
(define (maxi alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else (cond
            [(> (first alon)
                (maxi (rest alon)))
             (first alon)]
            [else
             (maxi (rest alon))])]))


(define (sampler rel-op alon)
  (cond
    [(empty? (rest alon)) (first alon)]
    [else (cond
            [(rel-op (first alon)
                     (sampler rel-op (rest alon)))
             (first alon)]
            [else
             (sampler rel-op (rest alon))])]))

(define (sampler1 rel-op alon)
    (cond
      [(empty? (rest alon)) (first alon)]
      [else (local ((define first-item (first alon))
                    (define rest-filtered (sampler1 rel-op (rest alon)))
                    (define (selector x y)
                      (cond
                        [(rel-op x y) x]
                        [else y])))
              (selector first-item rest-filtered))]))

(define (min1 alon)
  (sampler1 < alon))

(define (max1 alon)
  (sampler1 > alon))

(define list1 (list 3 7 6 2 9 8))
(define list2 (list 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define list3 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(check-expect (min1 list1) 2)
(check-expect (min1 list2) 1)
(check-expect (min1 list3) 1)
(check-expect (max1 list1) 9)
(check-expect (max1 list2) 20)
(check-expect (max1 list3) 20)

;; 느려진 이유 : 재귀에서 비교 조건이 계속해서 만족하지 못할경우 
;; 남은 최소/최대값을 계속해서 다시 탐색하게 된다.
;; sampler1 같이 재호출이 없는 상황이라면 n번에 가능


;; ex 19.1.6
;; sort1 : list-of-numbers  ->  list-of-numbers
(define (sort1 rel-op alon)
  (local ((define (sort alon)
            (cond
              [(empty? alon) empty]
              [else (insert (first alon) (sort (rest alon)))]))
          (define (insert an alon)
            (cond
              [(empty? alon) (list an)]
              [else (cond
                      [(rel-op an (first alon)) (cons an alon)]
                      [else (cons (first alon) (insert an (rest alon)))])])))
    (sort alon)))

(check-expect (sort1 < (list 2 3 1 5 4)) (list 1 2 3 4 5))
(check-expect (sort1 > (list 2 3 1 5 4)) (list 5 4 3 2 1))


;; ch 19.2

(define-struct ir (name price))

(define (below-ir1 aloir t)
  (filter1 <ir aloir t))

(define (<ir ir p)
  (< (ir-price ir) p))

;; find : loIR symbol  ->  boolean
;; aloir에 이름이 t인 항목이 존재하는지 검사한다.
(define (find aloir t)
  (cons? (filter1 eq-ir? aloir t)))

;; eq-ir? : IR symbol  ->  boolean
;; ir의 이름을 p와 비교한다.
(define (eq-ir? ir p)
  (symbol=? (ir-name ir) p))


;; ex 19.2.1
(define ir1 (list (make-ir 'doll 8) (make-ir 'robot 12)))
(define ir2 (list (make-ir 'doll 8) (make-ir 'doll 13) (make-ir 'robot 12)))

(check-expect (below-ir1 ir1 10) 
              (list (make-ir 'doll 8)))
(check-expect (find ir2 'doll)
              true)


;; length : (listof X)  ->  number
;; 리스트의 길이를 계산한다.
(define (length1 alon)
  (cond
    [(empty? alon) empty]
    [else (+ (length1 (rest alon)) 1)]))


;; ex 19.2.2
(define (sort-asc-ir ir1 ir2)
  (< (ir-price ir1) (ir-price ir2)))

(define (sort-desc-ir ir1 ir2)
  (> (ir-price ir1) (ir-price ir2)))

(check-expect (sort1 sort-asc-ir ir2)
              (list (make-ir 'doll 8) (make-ir 'robot 12) (make-ir 'doll 13)))
(check-expect (sort1 sort-desc-ir ir2)
              (list (make-ir 'doll 13) (make-ir 'robot 12) (make-ir 'doll 8)))


;; ex 19.2.3
(define-struct pair (left right))

(define pairs1 (list (make-pair 3 31) (make-pair 2 28) (make-pair 12 25)))
(define pairs2 (list (make-pair 'foo 1) (make-pair 'bar 2)))
(define pairs3 (list (make-pair 'alex 'jeon) (make-pair 'bobby 'kim)))

(define (lefts alop)
  (cond
    [(empty? alop) empty]
    [else
     (cons (pair-left (first alop))
           (lefts (rest alop)))]))

(check-expect (lefts pairs1) (list 3 2 12))
(check-expect (lefts pairs2) (list 'foo 'bar))
(check-expect (lefts pairs3) (list 'alex 'bobby))


;; ex 19.2.4
(define (last aloi)
  (cond
    [(empty? (rest aloi)) (first aloi)]
    [else (last (rest aloi))]))

(check-expect (last (list 1 2 3 4)) 4)
