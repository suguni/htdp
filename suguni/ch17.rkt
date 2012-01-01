;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
