;; ch 8

;; ex 8.2.1
;; 올바른 표현
;; 1. x
;; 표현(<exp>)은 <var> 만으로도 구성될 수 있다.
;; 2. (= y z)
;; (<prn> <var> <var>) 형태이므로 올바르다.
;; 3. (= (= y z) 0)
;; (<prn> <exp> <con>) 형태이고, 안쪽에 있는 <exp>는 다시 (<prn> <var> <var>) 형태이므로 올바르다.

;; 올바르지 않은 표현
;; 1. (3 + 4)
;; <prn>(+)가 앞에 와야 한다.
;; 2. empty? (l)
;; <prn>(empty?) 앞에 시작하는 괄호( ( )가 와야 한다.
;; 3. (x)
;; <var>(x)는 괄호 없이 독립적으로 존재하던지,
;; (<var> <exp> ...) 형태여야 하는데 뒤에 <exp>가 없다.

;; ex. 8.2.2
;; 올바른 표현
;; 1. (define (f x) x)
;; (define (<var> <var>) <var>) 형태이므로 올바르다.
;; 2. (define (f x) y)
;; (define (<var> <var>) <var>) 형태이므로 올바르다.
;; 3. (define (f x y) 3)
;; (define (<var> <var> <var>) <con>) 형태이므로 올바르다.

;; 올바르지 않은 표현
;; 1. (define (f 'x) x)
;; <var>가 와야 할 자리에 <con>('x)가 있다.
;; 2. (define (f x y z) (x))
;; body에 해당하는 <exp>가 잘못되었다. ex 8.2.1에서 올바르지 않은 표현 3과 동일한 이유.
;; 3. (define (f) 10)
;; (define (<var> <var> ...) <exp>) 에서 <var>가 2개 이상 있어야 하는데 하나(f)밖에 없다.

;; ex 8.2.3
;; 1. (x)
;; 올바르지 않은 문장. ex 8.2.1 참조
;; 2. (+ 1 (not x))
;; (<prn> <con> (<prn> <var>)) 형태로 올바르다.
;; 3. (+ 1 2 3)
;; (<prn> <con> <con> <con>) 형태로 올바른 문장이다.

;; ex 8.2.4
;; 1. (define (f x) 'x)
;; (define (<var> <var>) <exp>)에서 <exp>가 <con>인 올바른 문장
;; 2. (define (f 'x) x)
;; <var>가 있어야 할 자리에 <con>('x)이 있다.
;; 3. (define (f x y) (+ 'y (not x)))
;; (define (<var> <var> <var>) (<prn> <con> (<prn> <var>))) 형태로 올바른 문장

;; ex 8.3.1
;; 1.
(+ (* (/ 12 8) 2/3)
   (- 20 (sqrt 4)))
(+ (* 3/2 2/3)
   (- 20 2))
(+ 1 18)
19

;; 2.
(cond
 [(= 0 0) false]
 [(> 0 1) (symbol=? 'a 'a)]
 [else (= (/ 1 0) 9)])
(cond
 [true false]
 ...)
false

;; 3.
(cond
 [(= 2 0) false]
 [(> 2 1) (symbol=? 'a 'a)]
 [else (= (/ 1 2) 9)])
(cond
 [false false]
 [(> 2 1) (symbol=? 'a 'a)]
 ...)
(cond
 [(> 2 1) (symbol=? 'a 'a)]
 ...)
(cond
 [true (symbol=? 'a 'a)]
 ...)
(symbol=? 'a 'a)
true

;; ex 8.3.2
;; f : number number -> number
(define (f x y)
  (+ (* 3 x) (* y y)))

;; 1.
(+ (f 1 2) (f 2 1))
(+ (+ (* 3 1) (* 2 2))
   (+ (* 3 2) (* 1 1)))
(+ (+ 3 4)
   (+ 6 1))
(+ 7 7)
14

;; 2.
(f 1 (* 2 3))
(f 1 6)
(+ (* 3 1) (* 6 6))
(+ 3 36)
39

;; 3.
(f (f 1 (* 2 3)) 19)
(f (f 1 6) 19)
(f (+ (* 3 1) (* 6 6)) 19)
(f (+ 3 36) 19)
(f 39 19)
(+ (* 3 39) (* 19 19))
(+ 117 361)
478

;; ex 8.6.1
(define MY-PI 3.14159)
(define WIDTH 10)
(define HEIGHT 20)
(define AREA-RECTANGLE (* WIDTH HEIGHT))
(define AREA-CIRCLE (* MY-PI WIDTH WIDTH))

;; ex 8.6.2
(define RADIUS 10)
(define DIAMETER (* 2 RADIUS)) ;; 20
(define CIRCUMFERENCE (* 3.14 DIAMETER)) ;; 62.8

;; ex 8.6.3
(define PRICE 5)
(define SALES-TAX (* .08 PRICE)) ;; (* .08 5) = .4
(define TOTAL (+ PRICE SALES-TAX)) ;; (+ 5 .4) = 5.4

;; ex 8.7.1
;; (define-struct personnel-record (name salary dob ssn))
;; 올바름.

;; (define-struct oops ())
;; 올바르지 않음. 괄호로 묶임 이름 영역에 var들이 없음. X!!!
;; REPL에 돌려보면 올바른 표현임.

;; (define-struct child (dob date (- date dob)))
;; (- date dob)은 var가 아님.

;; (define-struct (child person) (dob date))
;; 이름이 var가 아님.

;; (define-struct child (parents dob date))
;; 올바름.

;; ex 8.7.2
(define-struct point (x y z))
(make-point 1 2 3)
(make-point (make-point 1 2 3) 4 5)
(make-point (+ 1 2) 3 4)
;; 모두 값이다.

;; ex 8.7.3
(define-struct ball (x y speed-x speed-y))

;; 1.
(number? (make-ball 1 2 3 4))
;; => false
;; 2.
(ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3))
;; => 3
;; 3.
(ball-y (make-ball (+ 1 2) (+ 3 3) 2 3))
;; => 6

;; 1.
(number? (make-ball 1 3 4)) ;; error, 인자 개수가 부족
;; 2.
(ball-x (make-posn 1 2)) ;; error, 인자가 ball이 아님
;; 3.
(ball-speed-y 5) ;; error, 인자가 ball이 아님
