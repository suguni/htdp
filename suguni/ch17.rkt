;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch17) (read-case-sensitive #t) (teachpacks ((lib "hangman.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hangman.ss" "teachpack" "htdp")))))
;; replace-eol-with : list-of-numbers list-of-numbers -> list-of-numbers
(define (replace-eol-with alon1 alon2)
  (cond
    [(empty? alon1) alon2]
    [else (cons (first alon1) (replace-eol-with (rest alon1) alon2))]))

;; tests
(define L (cons 1 (cons 2 empty)))
(check-expect (replace-eol-with empty L) L)
(check-expect (replace-eol-with (cons 1 empty) L)
              (cons 1 L))

;; ex17.1.1
;; append : list list list -> list
(define (our-append l1 l2 l3)
  (replace-eol-with (replace-eol-with l1 l2) l3))

;; tests
(check-expect (our-append (list 'a) (list 'b 'c) (list 'd 'e 'f)) (list 'a 'b 'c 'd 'e 'f))

;; ex 17.1.2
;; cross : list-of-symbols list-of-numbers -> list-of-lists
;; The function consumes a list of symbols and a list of numbers and produces all possible pairs of symbols and numbers.
(define (cross los lon)
  (cond
    [(empty? los) empty]
    [else (replace-eol-with (cross-a (first los) lon)
                            (cross (rest los) lon))]))

;; cross-a : symbol list-of-numbers -> list-of-lists
(define (cross-a s lon)
  (cond
    [(empty? lon) empty]
    [else (cons (list s (first lon))
                (cross-a s (rest lon)))]))

;; tests
(check-expect (cross '(a b c) '(1 2))
              (list (list 'a 1) (list 'a 2) (list 'b 1) (list 'b 2) (list 'c 1) (list 'c 2)))
(check-expect (cross-a 'a '(1 2))
              (list (list 'a 1) (list 'a 2)))

;; 17.2
;; hours->wages : list-of-numbers list-of-numbers -> list-of-numbers
(define (hours->wages alon1 alon2)
  (cond
    [(empty? alon1) empty]
    [else (cons (* (first alon1) (first alon2))
                (hours->wages (rest alon1) (rest alon2)))]))

;; tests
(check-expect (hours->wages empty empty) empty)
(check-expect (hours->wages (cons 5.65 empty) (cons 40 empty))
              (cons 226.0 empty))
(check-expect (hours->wages (cons 5.65 (cons 8.75 empty))
                            (cons 40.0 (cons 30.0 empty)))
              (cons 226.0 (cons 262.5 empty)))

;; ex 17.2.1

;; employee structure
(define-struct employee (name ssn pay-rate))

;; work structure
(define-struct work (employee-name hours-in-week))

;; week-wage structure
(define-struct weekly-wage (employee-name wage-per-week))

;; hours->wages-2 : list-of-employees list-of-works -> list-of-weekly-wage
(define (hours->wages-2 loe low)
  (cond
    [(empty? loe) empty]
    [else
     (cons (make-weekly-wage (employee-name (first loe))
                             (* (employee-pay-rate (first loe))
                                (work-hours-in-week (first low))))
           (hours->wages-2 (rest loe) (rest low)))]))

;; tests
(define emp-a (make-employee 'a 1111 5.65))
(define emp-b (make-employee 'b 1112 8.75))
(define work-a (make-work 'a 40.0))
(define work-b (make-work 'b 30.0))
(check-expect (hours->wages-2 empty empty) empty)
(check-expect (hours->wages-2 (cons emp-a empty) (cons work-a empty))
              (cons (make-weekly-wage 'a 226.0) empty))
(check-expect (hours->wages-2 (cons emp-a (cons emp-b empty))
                            (cons work-a (cons work-b empty)))
              (cons (make-weekly-wage 'a 226.0)
                    (cons (make-weekly-wage 'b 262.5) empty)))

;; ex 17.2.2
(define-struct phone-record (name number))

;; zip : list-of-symbols(name) list-of-symbols(phone number) -> list-of-phone-records
(define (zip lon lopn)
  (cond
    [(empty? lon) empty]
    [else (cons (make-phone-record (first lon) (first lopn))
                (zip (rest lon) (rest lopn)))]))

;; tests
(check-expect (zip (cons 'Steve (cons 'Bizen empty))
                   (cons '010-4206-2688 (cons '010-4847-0337 empty)))
              (cons (make-phone-record 'Steve '010-4206-2688)
                    (cons (make-phone-record 'Bizen '010-4847-0337) empty)))

;; 17.3
;; list-pick : list-of-symbols n[>=1] -> symbol
;(define (list-pick alos n)
;  (cond
;    [(and (= n 1) (empty? alos)) (error 'list-pick "list too short")]
;    [(and (> n 1) (empty? alos)) (error 'list-pick "list too short")]
;    [(and (= n 1) (cons? alos)) (first alos)]
;    [(and (> n 1) (cons? alos)) (list-pick (rest alos) (sub1 n))]))

;; apply ch 17.5 function simplication
(define (list-pick alos n)
  (cond
    [(empty? alos) (error 'list-pick "list too short")]
    [(= n 1) (first alos)]
    [(> n 1) (list-pick (rest alos) (sub1 n))]))

;; tests
(check-error (list-pick empty 1))
(check-expect (list-pick (cons 'a empty) 1) 'a)
(check-error (list-pick empty 3))
(check-error (list-pick (cons 'a empty) 3))

;; ex 17.3.1
;; list-pick0 : list-of-symbols n[>=0] -> symbol
;(define (list-pick0 alos n)
;  (cond
;    [(and (= n 0) (empty? alos)) (error 'list-pick "list too short")]
;    [(and (> n 0) (empty? alos)) (error 'list-pick "list too short")]
;    [(and (= n 0) (cons? alos)) (first alos)]
;    [(and (> n 0) (cons? alos)) (list-pick0 (rest alos) (sub1 n))]))

;; tests
(check-expect (list-pick0 (list 'a 'b 'c 'd) 3) 'd)
(check-error (list-pick0 (list 'a 'b 'c 'd) 4))

(check-error (list-pick0 empty 0))
(check-expect (list-pick0 (cons 'a empty) 0) 'a)
(check-error (list-pick0 empty 2))
(check-error (list-pick0 (cons 'a empty) 2))

;; ex 17.4.1
;; 17.2절의 절략이라는 말이 무슨 의미인지 모르겠음.
;; hours->wages는 두 입력인자의 크기(리스트 길이)가 같은 경우,
;; cond 조건절에서 한 인자의 상태(empty?)만 검사하면 된다는 것이었는데,
;; replace-eol-with의 경우 두번째 인자가 리스트이기만 하면 되므로(길이에 무관),
;; 동일한 전략(?)을 사용할 수 없는것 아닌가?

;; ex 17.4.2
;; simplified version of list-pick0
(define (list-pick0 alos n)
  (cond
    [(empty? alos) (error 'list-pick "list too short")]
    [(= n 0) (first alos)]
    [(> n 0) (list-pick0 (rest alos) (sub1 n))]))

;; ex 17.6.1
;; merge : list-of-numbers list-of-numbers -> list-of-numbers
(define (merge lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(empty? lon2) lon1]
    [(< (first lon1) (first lon2))
     (cons (first lon1) (merge (rest lon1) lon2))]
    [(>= (first lon1) (first lon2))
     (cons (first lon2) (merge lon1 (rest lon2)))]))

;; tests
(check-expect (merge (list 1 3 5 7 9) (list 0 2 4 6 8))
              (list 0 1 2 3 4 5 6 7 8 9))
(check-expect (merge (list 1 8 8 11 12) (list 2 3 4 8 13 14))
              (list 1 2 3 4 8 8 8 11 12 13 14))

;; ex 17.6.2

;; 글자(letter)는 'a 부터 'z 중 한 기호이다.

;; 단어(word)는 다음 두 가지 중 하나이다.
;; 1. empty : 빈 리스트
;; 2. (cons l w) : l은 글자, w는 단어

;; 상태글자(status-letter)는 글자(letter) 또는 기호 '_ 이다.

;; 상태단어(status-word)는 다음 두 가지 중 하나이다.
;; 1. empty : 빈 리스트
;; 2. (cons sl sw) : sl은 상태글자, sw는 상태단어

;; reveal-list : word status-word letter -> status-word
(define (reveal-list w sw l)
  (cond
    [(empty? w) empty]
    [else
     (cons (reveal-one (first w) (first sw) l)
           (reveal-list (rest w) (rest sw) l))]))

;; tests
(check-expect (reveal-list (list 't 'e 'a) (list '_ 'e '_) 'u)
              (list '_ 'e '_))
(check-expect (reveal-list (list 'a 'l 'e) (list 'a '_ '_) 'e)
              (list 'a '_ 'e))
(check-expect (reveal-list (list 'a 'l 'l) (list '_ '_ '_) 'l)
              (list '_ 'l 'l))

;; reveal-one : letter status-letter letter -> status-letter
(define (reveal-one chosen status guess)
  (cond
    [(symbol=? status '_)
     (cond
       [(symbol=? chosen guess) guess]
       [else '_])]
    [else status]))

;; tests
(check-expect (reveal-one 'a '_ 'c) '_)
(check-expect (reveal-one 'a '_ 'a) 'a)
(check-expect (reveal-one 'a 'a 'x) 'a)

;; draw-next-part from ex 6.7.1
(define (draw-next-part part)
  (cond
    [(symbol=? part 'head)
     (draw-circle (make-posn 100 50) 10 'black)]
    [(symbol=? part 'body)
     (draw-solid-line (make-posn 100 60) (make-posn 100 130) 'black)]
    [(symbol=? part 'right-arm)
     (draw-solid-line (make-posn 100 75) (make-posn 160 65) 'black)]
    [(symbol=? part 'left-arm)
     (draw-solid-line (make-posn 100 75) (make-posn 40 65) 'black)]
    [(symbol=? part 'right-leg)
     (draw-solid-line (make-posn 100 130) (make-posn 165 160) 'black)]
    [(symbol=? part 'left-leg)
     (draw-solid-line (make-posn 100 130) (make-posn 35 160) 'black)]
    [(symbol=? part 'noose)
     (and
      (draw-solid-line (make-posn 0 10) (make-posn 100 10) 'black)
      (draw-solid-line (make-posn 100 10) (make-posn 100 30) 'black)
      (draw-circle (make-posn 120 50) 30 'red)
      (draw-solid-line (make-posn 115 35) (make-posn 125 45))
      (draw-solid-line (make-posn 125 35) (make-posn 115 45))
      (draw-solid-line (make-posn 130 40) (make-posn 140 50))
      (draw-solid-line (make-posn 140 40) (make-posn 130 50)))]
    [else false]))

;; test
;; (and (start 200 300) (hangman-list reveal-list draw-next-part))

;; ex 17.6.3

;; employee-record : name, employee-number, pay-rate
;; (define-struct employee (name ssn pay-rate))

;; punch-card : employee-number, worked-hours
(define-struct punch-card (ssn work-hours))

;; hours->wages2 : list-of-employee-records, list-of-punch-cards -> weekly-wage-for-employee
;; if employee and punch card are mismatched, function stop with an error message.
(define (hours->wages2 loe lop)
  (cond
    [(empty? loe) empty]
    [else
     (hours->wages2-sorted (sort-loe-by-ssn loe)
                           (sort-lop-by-ssn lop))]))

(define (hours->wages2-sorted loe lop)
  (cond
    [(empty? loe) empty]
    [(not (= (employee-ssn (first loe))
             (punch-card-ssn (first lop)))) (error "hours->wages2-sorted")]
    [else
     (cons (* (employee-pay-rate (first loe))
              (punch-card-work-hours (first lop)))
           (hours->wages2-sorted (rest loe) (rest lop)))]))

;; sort list of employee-record by ssn
;; sort-loe-by-ssn : list-of-employ-record -> sorted list-of-employ-record
(define (sort-loe-by-ssn loe)
  (cond
    [(empty? loe) empty]
    [else
     (insert-loe-by-ssn (first loe) 
                        (sort-loe-by-ssn (rest loe)))]))

;; insert empolyee-record to sorted list-of-employee sorted manner
;; insert-loe-by-ssn : employee-record sorted list-of-employee-reocrd -> sorted list-of-employee-record
(define (insert-loe-by-ssn emp loe)
  (cond
    [(empty? loe) (list emp)]
    [else
     (cond
       [(>= (employee-ssn emp) (employee-ssn (first loe)))
        (cons emp loe)]
       [else
        (cons (first loe)
              (insert-loe-by-ssn emp (rest loe)))])]))

;; sort list of punch-card by ssn
;; sort-lop-by-ssn : list-of-punch-cards -> sorted list-of-punch-cards
(define (sort-lop-by-ssn lop)
  (cond
    [(empty? lop) empty]
    [else
     (insert-lop-by-ssn (first lop) 
                        (sort-lop-by-ssn (rest lop)))]))

;; insert punch-card to sorted list-of-punch-cards sorted manner
;; insert-lop-by-ssn : punch-card list-of-punch-cards(sorted) -> list-of-punch-cards(sorted)
(define (insert-lop-by-ssn card lop)
  (cond
    [(empty? lop) (list card)]
    [else
     (cond
       [(>= (punch-card-ssn card) (punch-card-ssn (first lop)))
        (cons card lop)]
       [else
        (cons (first lop)
              (insert-lop-by-ssn card (rest lop)))])]))
;; tests

;; list of employee-records
(define loe
  (list
   (make-employee 'b 2 5)
   (make-employee 'a 1 10)
   (make-employee 'c 3 12)
   (make-employee 'd 4 16)))
(define sorted-loe
  (list
   (make-employee 'd 4 16)
   (make-employee 'c 3 12)
   (make-employee 'b 2 5)
   (make-employee 'a 1 10)))

;; list of punch-cards
(define lop
  (list
   (make-punch-card 4 40)
   (make-punch-card 3 48)
   (make-punch-card 1 36)
   (make-punch-card 2 23)))
(define sorted-lop
  (list
   (make-punch-card 4 40)
   (make-punch-card 3 48)
   (make-punch-card 2 23)
   (make-punch-card 1 36)))

(check-expect (hours->wages2-sorted sorted-loe sorted-lop)
              (list (* 16 40)   ;; 'd 4
                    (* 12 48)   ;; 'c 3
                    (* 5 23)    ;; 'b 2
                    (* 10 36))) ;; 'a 1

(check-expect (hours->wages2 loe lop)
                (list (* 16 40)     ;; 'd 4
                      (* 12 48)     ;; 'c 3
                      (* 5 23)      ;; 'b 2
                      (* 10 36)))   ;; 'a 1

(check-expect (insert-loe-by-ssn
               (make-employee 'c 3 12)
               (list
                (make-employee 'd 4 16)
                (make-employee 'b 2 5)
                (make-employee 'a 1 10)))
              sorted-loe)
              
(check-expect (sort-loe-by-ssn loe) sorted-loe)
(check-expect (sort-lop-by-ssn lop) sorted-lop)

;; 흐흐... 정렬 먼저하라고 해서 했는데 코드 중복이 많다.

;; ex 17.6.4
