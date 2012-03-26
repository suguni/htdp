;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ch23) (read-case-sensitive #t) (teachpacks ((lib "gui.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "gui.ss" "teachpack" "htdp")))))
;; ch23

;; make-even : N->N[even]
(define (make-even n)
  (* 2 n))

;; make-odd : N->N[odd]
(define (make-odd n)
  (+ (* 2 n) 1))

;; series-even : N->number
(define (series-even n)
  (cond
    [(= n 0) (make-even n)]
    [else (+ (make-even n)
             (series-even (- n 1)))]))

;; tests
(check-expect (series-even 9) 90)

;; series-odd : N->number
(define (series-odd n)
  (cond
    [(= n 0) (make-odd n)]
    [else (+ (make-odd n)
             (series-odd (- n 1)))]))

;; tests
(check-expect (series-odd 9) 100)

;; series : N (N->numbers) -> number
(define (series n a-term)
  (cond
    [(= n 0) (a-term n)]
    [else (+ (a-term n)
             (series (- n 1) a-term))]))

;; tests
(check-expect (series 9 make-even) 90)
(check-expect (series 9 make-odd) 100)

;; ex 23.1.1
;; series-local : (N->number) -> (N->number)
(define (series-local a-term)
  (local ((define (series n)
            (cond
              [(= n 0) (a-term n)]
              [else (+ (a-term n)
                       (series (- n 1)))])))
    series))

(define series-even2 (series-local make-even))
(define series-odd2 (series-local make-odd))

;; tests
(check-expect (series-even2 9) 90)
(check-expect (series-odd2 9) 100)

;; ch 23.2
;; 책에서 설명은 첫항이 3이고 더하는 상수가 5라고 되어 있는데,
;; 그림(예)에서는 첫항이 8로 되어 있다. 아래는 3부터 시작하도록 한다.

;; ex 23.2.1
;; a-fives number->number
(define (a-fives n)
  (cond
    [(= n 0) 3]
    [else (+ 5 (a-fives (- n 1)))]))

;; tests
(check-expect (a-fives 0) 3)
(check-expect (a-fives 3) 18)

;; ex 23.2.2
(define (a-fives-closed n)
  (+ (* 5 n) 3))

;; tests
(check-expect (a-fives-closed 0) 3)
(check-expect (a-fives-closed 3) 18)

;; 2.1과 2.2에 대한 이해
;; 재귀적 방법 - n값을 하나씩 감소시켜가면서 5를 더한다.
;; 비재귀적 방법 - 주어진 수열의 계산 식에 따라 한번에 계산한다.

;; ex 23.2.3
(check-expect (series 3 a-fives-closed) 42)
(check-expect (series 7 a-fives-closed) 164)
(check-expect (series 88 a-fives-closed) 19847)
;; 무한 등차 수열의 합은 존재할 수 있는가?
;; 위 수열의 경우 수렴하지 않으므로 존재하지 않는다.

;; ex 23.2.4
(define (seq-a-fives n)
  (build-list (+ n 1) a-fives-closed))

;; tests
(check-expect (seq-a-fives 3) (list 3 8 13 18))

;; ex 23.2.5
;; arithmetic-series number number->(N->number)
(define (arithmetic-series start s)
  (local ((define (term n)
            (+ (* n s) start)))
    term))

;; tests
;; (arithmetic-series 3 5) === a-fives
(check-expect ((arithmetic-series 3 5) 3) 18)
(check-expect ((arithmetic-series 0 2) 3) 6)

;; (arithmetic-series 3 5)는 a-fives와 같으므로 이같이 풀긴 했는데,
;; 앞에서 series라는 함수는 입력한 n번째 항목까지의 합을 계산하는 함수 아닌가?

;; ch23.3
