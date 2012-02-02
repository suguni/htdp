;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch15) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))
(define-struct wp (header body))

(define wp1 (make-wp 'top-title (list 'hello 'top 'page)))
(define wp2 (make-wp 'sub1-title (list 'hello 'sub1 'page wp1 'hi 'sub1)))

(define (find s wp)
  (cond
    [(empty? (find-sub s wp)) false]
    [else (find-sub s wp)]))

(define (find-sub s wp)
  (cond
    [(find? s (wp-body wp))
     (cons (wp-header wp)
           (find-wp s (wp-body wp)))]
    [else (find-wp s (wp-body wp))]))

(define (find-wp s wd)
  (cond
    [(empty? wd) empty]
    [(symbol? (first wd)) (find-wp s (rest wd))]
    [else (append (find-sub s (first wd))
                  (find-wp s (rest wd)))]))

(define (find? s wd)
  (cond
    [(empty? wd) false]
    [(symbol? (first wd))
     (or (symbol=? s (first wd))
         (find? s (rest wd)))]
    [else (find? s (rest wd))]))

;; tests
(check-expect (find? 'top (wp-body wp1)) true)
(check-expect (find 'top wp1) (list 'top-title))
(check-expect (find 'top wp2) (list 'top-title))
(check-expect (find 'sub1 wp2) (list 'sub1-title))
(check-expect (find 'hello wp1) (list 'top-title))
(check-expect (find 'page wp1) (list 'top-title))
(check-expect (find 'hi wp1) false)
(check-expect (find 'hi wp2) (list 'sub1-title))
(check-expect (find 'good wp2) false)
(check-expect (find 'page wp2) (list 'sub1-title 'top-title))