;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter.10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;ex.10.1.7
(define (recall ty lon)
  (cond [(empty? lon) empty]
        [else (cond [(symbol=? ty (first lon)) (recall ty (rest lon))]
                    [else (cons (first lon) (recall ty (rest lon)))])]))

(recall 'robot (cons 'robot (cons 'doll (cons 'dress empty))))

;ex.10.1.8
(define (quadratic-roots a b c)
  (cond [(= a 0) 'degenerate]
        [(< (sqr b) (* 4 a c)) 'none]
        [(= (sqr b) (* 4 a c)) (* -0.5 (/ b a))]
        [else (cons (/ (+ (* -1 b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a)) 
                    (cons (/ (- (* -1 b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a)) empty))]))

(quadratic-roots 1 0 -1)

;ex.10.1.9
;(define (controller money)

;ex.10.2.1
(define-struct ir (name price))