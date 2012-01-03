;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter.12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (sort alon)
 (cond
   [(empty? alon) empty]
   [(cons? alon) (insert (first alon) (sort (rest alon)))]))
(define (insert n alon)
 (cond
   [(empty? alon) (cons n empty)]
   [(< n (first alon)) (cons (first alon) (insert n (rest alon)))]
   [(>= n (first alon)) (cons n alon)]))
;;ex.12.2.1
(define-struct mail (from date message))
(define (order-by-date mail-list)
 (cond
   [(empty? mail-list) empty]
   [(cons? mail-list) (insert (mail-date (first mail-list)) (order-by-date (rest mail-list)))]))
(define (insert-s s alon)
 (cond 
   [(empty? alon) (cons s empty)]
   [
