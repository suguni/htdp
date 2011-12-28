;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; hours->wages : list-of-numbers  ->  list-of-numbers
;; to create a list of weekly wages from a list of weekly hours (alon)
(define (hours->wages alon)
  (cond
    [(empty? alon) empty]
    [else (cons (wage (first alon)) (hours->wages (rest alon)))]))

;; wage : number  ->  number
;; to compute the total wage (at $12 per hour)
;; of someone who worked for h hours
(define (wage h)
  (* 12 h))

;; ex 10.1.1
;wage 함수의 12를 14로 변경

;; ex 10.1.2
(define (test-hours->wages alon)
  (cond
    [(> (first alon) 100) (error 'test-hours->wages "too many hours")]
    [(empty? alon) empty]
    [else (cons (wage (first alon)) (test-hours->wages (rest alon)))]))

;; ex 10.1.3
(define (Fahrenheit->Celsius f)
  (- (/ (+ f 40) 1.8) 40))

(define (convertFC alon)
  (cond
    [(empty? alon) empty]
    [else (cons (Fahrenheit->Celsius (first alon)) (convertFC (rest alon)))]))

;; ex 10.1.4
(define (exchange usd)
  (* 1.22 usd))

(define (convert-euro alon)
  (cond
    [(empty? alon) empty]
    [else (cons (exchange (first alon)) (convert-euro (rest alon)))]))

(define (exchange-by-ratio ratio usd)
  (* ratio usd))

(define (convert-euro-1 ratio alon)
  (cond
    [(empty? alon) empty]
    [else (cons (exchange-by-ratio ratio (first alon)) (convert-euro-1 ratio (rest alon)))]))


;; ex 10.1.5
(define (eliminate-exp ua lotp)
  (cond
    [(empty? lotp) empty]
    [else (cond
            [(>= ua (first lotp)) (cons (first lotp) (eliminate-exp ua (rest lotp)))]
            [else (eliminate-exp ua (rest lotp))])]))

(check-expect (eliminate-exp 1.0 (cons 2.95 (cons .95 (cons 1.0 (cons 5 empty)))))
              (cons .95 (cons 1.0 empty)))

;; ex 10.1.6
;일반화한 함수만 작성
(define (substitute new old alos)
  (cond
    [(empty? alos) empty]
    [else (cons
           (cond
             [(symbol=? (first alos) old) new]
             [else (first alos)])
           (substitute new old (rest alos)))]))

(check-expect 
 (substitute 'Barbie 'doll (cons 'robot (cons 'doll (cons 'dress empty))))
 (cons 'robot (cons 'Barbie (cons 'dress empty))))

;; ex 10.1.7
(define (recall ty lon)
  (cond
    [(empty? lon) empty]
    [else (cond
             [(symbol=? (first lon) ty) (recall ty (rest lon))]
             [else (cons (first lon) (recall ty (rest lon)))])]))

(check-expect
 (recall 'robot (cons 'robot (cons 'doll (cons 'dress empty))))
 (cons 'doll (cons 'dress empty)))

;; ex 10.1.8
(define (what-kind a b c)
  (cond
    [(= a 0) 'degenerate]
    [(> (sqr b) (* 4 a c)) 'two]
    [(= (sqr b) (* 4 a c)) 'one]
    [else 'none]))

(define (quadratic-roots a b c)
  (cond
    [(symbol=? (what-kind a b c) 'two)
     (cons (/ (+ (* -1 b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a))
     (cons (/ (- (* -1 b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a)) empty))]
    [(symbol=? (what-kind a b c) 'one)
     (/ (* -1 b) (* 2 a))]
    [else (what-kind a b c)]))

(check-expect (quadratic-roots 1 2 3) 'none)
(check-expect (quadratic-roots 0 2 3) 'degenerate)
(check-expect (quadratic-roots 1 2 1) -1)
(check-within (first (quadratic-roots 4 1 -8)) 1.294 0.001)
(check-within (first (rest (quadratic-roots 4 1 -8))) -1.544 0.001)

;; ex 10.1.9
(define (controller c)
  (cons (quotient c 100) 
        (cons (cond
                [(= (quotient c 100) 1) 'dollar]
                [else 'dollars])
              (cons 'and
                    (cons (remainder c 100)
                          (cons (cond
                                  [(= (remainder c 100) 1) 'cent]
                                  [else 'cents])
                                empty))))))  

(check-expect (controller 103)
              (cons 1 (cons 'dollar (cons 'and (cons 3 (cons 'cents empty))))))

     
;;;;
(define-struct ir (name price))

;; ex 10.2.1
;; contains-doll? : inventory -> boolean
(define (contains-doll? an-inv)
  (cond
    [(empty? an-inv) false]
    [else (cond
            [(symbol=? (ir-name (first an-inv)) 'doll) true]
            [else (contains-doll? (rest an-inv))])]))

(check-expect 
 (contains-doll? (cons (make-ir 'arrow 30) (cons (make-ir 'doll 10) empty))) 
 true)

;; ex 10.2.2
