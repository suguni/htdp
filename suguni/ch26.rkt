#lang racket

(require test-engine/racket-tests)

;;; ch 26.

;1. 간단하게 풀 수 있는 문제란 무엇인가?
;2. 이에 해당하는 답은 무엇인가?
;3. 원래 문제보다 쉽게 풀 수 있는 문제를 어떻게 만들 것인가?
;   우리가 만드는 새로운 문제는 한 개인가, 혹은 여러 개인가?
;4. 주어진 문제의 답이 새로운 문제들 중 하나의 답과 일치하는가?
;   아니면 이것들을 조합해서 원래 문제에 대한 답을 만들어야 하는가?
;   만일 그렇다면, 원래 문제 데이터에서 필요한 것이 있는가?

;ex 26.0.7
;1. 공이 화면을 벗어났는지 판단하는 함수. out-of-bound?
;2. true
;3. move-ball 함수로 새로운 문제를 만든다. 새로운 문제는 한개임.
;4. move-ball 함수로 만들어진 문제는 원래 문제와 본질적으로 동일한 것임. 결과를 조합할 필요 없다.

;ex 26.0.8
;1. 문제가 empty 인지 검사하는 것.
;2. 문제가 empty 이면 empty를 반환한다.
;3. 문제(list of numbers)의 첫번째 값을 pivot으로 하고, pivot보다 큰 값과 작은 값으로 나눈 2개의 문제(list of numbers)가 생성된다.
;4. 2개의 문제를 조합해야 하며, 작은 값과 큰 값의 목록을 구해야 한다.

;; ex 26.1.1
(define (tabulate-div n)
  (local ((define (divisor? d)
            (= (remainder n d) 0))
          (define (tab-div d)
            (cond
              [(= d 1) (list 1)]
              [(divisor? d) (cons d (tab-div (- d 1)))]
              [else (tab-div (- d 1))])))
    (tab-div n)))

(check-expect (tabulate-div 6) (list 6 3 2 1))

;; ex 26.1.2
(define (make-singles lon)
  (cond
    [(empty? lon) empty]
    [else (cons (list (first lon))
                (make-singles (rest lon)))]))
(check-expect (make-singles (list 2 5 9 3))
              (list (list 2) (list 5) (list 9) (list 3)))

(define (merge-neighbors l1 l2 result)
  (cond
    [(empty? l1) (append result l2)]
    [(empty? l2) (append result l1)]
    [(< (first l1) (first l2))
     (merge-neighbors (rest l1) l2 (append result (list (first l1))))]
    [else
     (merge-neighbors l1 (rest l2) (append result (list (first l2))))]))

(check-expect (merge-neighbors (list 2 5) (list 3 9) (list))
              (list 2 3 5 9))

(define (merge-all-neighbors lol)
  (cond
    [(empty? lol) empty]
    [(empty? (rest lol)) lol]
    [else (cons
           (merge-neighbors (first lol) (second lol) empty)
           (merge-all-neighbors (rest (rest lol))))]))

(check-expect (merge-all-neighbors (list (list 1)))
              (list (list 1)))
(check-expect (merge-all-neighbors (list (list 2) (list 5) (list 9) (list 3) (list 1)))
              (list (list 2 5) (list 3 9) (list 1)))
(check-expect (merge-all-neighbors (list (list 2 5) (list 3 9)))
              (list (list 2 3 5 9)))

(define (merge-sort lon)
  (local ((define (fn lol)
            (cond
              [(= (length lol) 0) empty]
              [(= (length lol) 1) (first lol)]
              [else (fn (merge-all-neighbors lol))])))
    (fn (make-singles lon))))

(check-expect (merge-sort (list 2 5 9 3))
              (list 2 3 5 9))

;; ex 26.2.1
(define (determine-solution p)
  0)

(define (combine-solutions p r)
  (+ r 1))

(define (generative-recursive-fun problem)
  (cond
    [(empty? problem) (determine-solution problem)]
    [else
     (combine-solutions
      problem
      (generative-recursive-fun (rest problem)))]))

(check-expect (generative-recursive-fun empty) 0)
(check-expect (generative-recursive-fun (list 1 2 3 4)) 4)

;; ch 26.3 selection

(define (gcd-structural n m)
  (local ((define (first-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else (cond
                      [(and (= (remainder n i) 0)
                            (= (remainder m i) 0))
                       i]
                      [else (first-divisor-<= (- i 1))])])))
    (first-divisor-<= (min n m))))

;; ex 26.3.1
;; > (time (gcd-structural 101135853 45014640))
;; cpu time: 16224 real time: 16217 gc time: 0
;; 177

(define (gcd-generative n m)
  (local ((define (clv-gcd larger smaller)
            (cond
              [(= smaller 0) larger]
              [else (clv-gcd smaller (remainder larger smaller))])))
    (clv-gcd (max n m) (min n m))))

;; ex 26.3.2
;1. (= smaller 0)
;2. larger
;3. (clv-gcd smaller (remainder larger smaller)
;4. 주어진 문제의 답이 새로운 문제의 답과 일치한다.


;; ex 26.3.3

;; > (time (gcd-generative 101135853 45014640))
;; cpu time: 0 real time: 0 gc time: 0
;; 177

;(clv-gcd 101135853 45014640)
;(clv-gcd 45014640 11106573)
;(clv-gcd 11106573 588348)
;(clv-gcd 588348 516309)
;(clv-gcd 516309 72039)
;(clv-gcd 72039 12036)
;(clv-gcd 12036 11859)
;(clv-gcd 11859 177)
;(clv-gcd 177 0)
;0

;; ex 26.3.4
;; 큰수를 작은 수로 나눈 나머지가 0이 되면 작은 수가 최대공약수가 된다.

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

;; ex 26.3.5
;(qs (list 10 6 8 9 14 12 3 11 14 16 2))
;
;(append (qs (list 6 8 9 3 2))
;        (list 10)
;        (qs (list 14 12 11 14 16)))
;
;(append (append (qs (list 3 2))
;                (list 6)
;                (qs (list 8 9)))
;        (list 10)
;        (append (qs (list 12 11 14))
;                (list 14)
;                (qs (list 16))))
;
;(append (append (append (qs (list 2))
;                        (list 3)
;                        (qs empty))
;                (list 6)
;                (append (qs empty)
;                        (list 8)
;                        (qs (list 9))))
;        (list 10)
;        (append (append (qs (list 11))
;                        (list 12)
;                        (qs (list 14)))
;                (list 14)
;                (list 16)))
;
;(append (append (append (list 2) (list 3) empty)
;                (list 6)
;                (append empty (list 8) (list 9)))
;        (list 10)
;        (append (append (list 11) (list 12) (list 14))
;                (list 14)
;                (list 16)))

;; qs - 12번
;; append - 6번

;(qs (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
;
;(append empty
;        (list 1)
;        (qs (list 2 3 4 5 6 7 8 9 10 11 12 13 14)))
;
;(append empty
;        (list 1)
;        (append empty
;                (list 2)
;                (qs (list 3 4 5 6 7 8 9 10 11 12 13 14))))
;
;(append empty
;        (list 1)
;        (append empty
;                (list 2)
;                (append empty
;                        (list 3)
;                        (qs (list 4 5 6 7 8 9 10 11 12 13 14)))))
;
;(append empty
;        (list 1)
;        (append empty
;                (list 2)
;                (append empty
;                        (list 3)
;                        (append empty
;                                (list 4)
;                                (qs (list 5 6 7 8 9 10 11 12 13 14))))))
;
;(append empty
;        (list 1)
;        (append empty
;                (list 2)
;                (append empty
;                        (list 3)
;                        (append empty
;                                (list 4)
;                                (append empty
;                                        (list 5)
;                                        (qs (list 6 7 8 9 10 11 12 13 14)))))))
;
;(append empty
;        (list 1)
;        (append empty
;                (list 2)
;                (append empty
;                        (list 3)
;                        (append empty
;                                (list 4)
;                                (append empty
;                                        (list 5)
;                                        (append empty
;                                                (list 6)
;                                                (qs (list 7 8 9 10 11 12 13 14))))))))
;
;(append empty
;        (list 1)
;        (append empty
;                (list 2)
;                (append empty
;                        (list 3)
;                        (append empty
;                                (list 4)
;                                (append empty
;                                        (list 5)
;                                        (append empty
;                                                (list 6)
;                                                (append empty
;                                                        (list 7)
;                                                        (qs (list 8 9 10 11 12 13 14)))))))))
;
;(append empty
;        (list 1)
;        (append empty
;                (list 2)
;                (append empty
;                        (list 3)
;                        (append empty
;                                (list 4)
;                                (append empty
;                                        (list 5)
;                                        (append empty
;                                                (list 6)
;                                                (append empty
;                                                        (list 7)
;                                                        (append empty
;                                                                (list 8)
;                                                                (qs (list 9 10 11 12 13 14))))))))))

;; ...

;(append empty
;        (list 1)
;        (append empty
;                (list 2)
;                (append empty
;                        (list 3)
;                        (append empty
;                                (list 4)
;                                (append empty
;                                        (list 5)
;                                        (append empty
;                                                (list 6)
;                                                (append empty
;                                                        (list 7)
;                                                        (append empty
;                                                                (list 8)
;                                                                (append empty
;                                                                        (list 9)
;                                                                        (append empty
;                                                                                (list 10)
;                                                                                (append empty
;                                                                                        (list 11)
;                                                                                        (append empty
;                                                                                                (list 12)
;                                                                                                (append empty
;                                                                                                        (list 13)
;                                                                                                        (list 14))))))))))))))

;; qs : 15번
;; append : 13번
;; 길이가 N인 리스트에 대해 qs는 N+1회 호출된다.
;; append 최대 N-1회 호출된다. ???

;; ex 26.3.6 - ???

;; from ch 12
;; sort : list-of-numbe(rs -> list-of-numbers
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

(define (create-tests n)
  (local ((define (rnd i lon)
            (cond
              [(= i 0) lon]
              [else (rnd (- i 1) (cons (random n) lon))])))
    (rnd n (list))))

(define test-case (create-tests 10000))
(collect-garbage)
(time (local ((define r (legacy-sort test-case)))
        (print "legacy-sort done => ")))
(collect-garbage)
(time (local ((define r (qs test-case)))
        (print "quick-sort done => ")))

;; 항상 quick-sort가 빠르다. 임계지점을 찾지 못하겠다.

