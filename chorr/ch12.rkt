;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch12) (read-case-sensitive #t) (teachpacks ((lib "draw.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.ss" "teachpack" "htdp")))))

;; sort : list-of-numbers  ->  list-of-numbers (sorted)
;; to create a list of numbers with the same numbers as
;; alon sorted in descending order
(define (sort alon)
  (cond
    [(empty? alon) empty]
    [(cons? alon) (insert (first alon) (sort (rest alon)))]))

;; insert : number list-of-numbers (sorted)  ->  list-of-numbers (sorted)
;; to create a list of numbers from n and the numbers on
;; alon that is sorted in descending order; alon is sorted
(define (insert n alon)
  (cond
    [(empty? alon) (cons n empty)]
    [else (cond
            [(>= n (first alon)) (cons n alon)]
            [(<  n (first alon)) (cons (first alon) (insert n (rest alon)))])]))

;; ex 12.2.1
(define-struct mail (from date message))

;; sort-mail-by-date : list-of-mail -> list-of-mail (sorted)
(define (sort-mail-by-date alom)
  (cond
    [(empty? alom) empty]
    [(cons? alom) (insert-mail-by-date (first alom) (sort-mail-by-date (rest alom)))]))

;; insert-mail-by-date : 
(define (insert-mail-by-date mail alom)
  (cond
    [(empty? alom) (cons mail empty)]
    [else (cond
            [(>= (mail-date mail) (mail-date (first alom))) 
             (cons mail alom)]
            [(<  (mail-date mail) (mail-date (first alom))) 
             (cons (first alom) (insert-mail-by-date mail (rest alom)))])]))

(define mail1 (cons (make-mail 'a 4 "message1")
                    (cons (make-mail 'b 1 "message2")
                          (cons (make-mail 'c 3 "message3") empty))))

(check-expect (sort-mail-by-date mail1)
              (cons
               (make-mail 'a 4 "message1")
               (cons (make-mail 'c 3 "message3") (cons (make-mail 'b 1 "message2") empty))))

;; ex 12.2.2

;; search : number list-of-numbers  ->  boolean
(define (search n alon)
  (cond
    [(empty? alon) false]
    [else (or (= (first alon) n) (search n (rest alon)))]))

;; search-sorted : number list-of-numbers -> boolean
(define (search-sorted n alon)
  (cond
    [(or (empty? alon) (> (first alon) n)) false]
    [(= (first alon) n) true]
    [else (search n (rest alon))]))

(check-expect (search-sorted 3 (cons 1 (cons 3 (cons 5 empty)))) true)
(check-expect (search-sorted 4 (cons 1 (cons 3 (cons 5 empty)))) false)

;; ch 12.3 --

;; draw-polygon : polygon -> true
(define (draw-polygon a-poly)
  (connect-dots (cons (last a-poly) a-poly)))

;; connect-dots : polygon -> true
(define (connect-dots a-poly)
  (cond
    [(empty? (rest a-poly)) true]
    [else (and (draw-solid-line (first a-poly) (second a-poly) 'RED)
               (connect-dots (rest a-poly)))]))

;; last : polygon -> posn
(define (last a-poly)
  (cond
    [(empty? (rest a-poly)) (first a-poly)]
    [else (last (rest a-poly))]))

(define poly1
  (cons (make-posn 10 10)
        (cons (make-posn 60 60)
              (cons (make-posn 10 60) empty))))

;(start 100 100)
;(draw-polygon poly1)

;; ex 12.3.1
;; draw-polygon-type-1 : polygon -> true
(define (draw-polygon-type-1 a-poly)
  (connect-dots (add-at-end (first a-poly) a-poly)))

;; add-at-end : posn polygon -> polygon
(define (add-at-end p a-poly)
  (cond
    [(empty? a-poly) (cons p empty)]
    [else (cons (first a-poly) (add-at-end p (rest a-poly)))]))

(check-expect (add-at-end (make-posn 1 1) poly1)
              (cons (make-posn 10 10)
                    (cons (make-posn 60 60)
                          (cons (make-posn 10 60) 
                                (cons (make-posn 1 1) empty)))))

;; ex 12.3.2
;; connect-dots : polygon posn -> true
(define (connect-dots-type-2 a-poly last-posn)
  (cond
    [(empty? (rest a-poly)) 
     (draw-solid-line (first a-poly) last-posn 'RED)]
    [else 
     (and (draw-solid-line (first a-poly) (second a-poly) 'RED)
          (connect-dots (rest a-poly)))]))

;; draw-polygon-type-2 : polygon -> true
(define (draw-polygon-type-2 a-poly)
  (connect-dots-type-2 a-poly (first a-poly)))

;; ex 12.4.1
;; arangements : word -> list-of-words
;; a-word에 들어 있는 글자를 재배치한 단어 리스트를 만든다.
(define (arrangements a-word)
  (cond
    [(empty? a-word) (cons empty empty)]
    [else (insert-everywhere/in-all-words (first a-word)
                                          (arrangements (rest a-word)))]))

;; ex 12.4.2
;; insert-everywhere/in-all-words : ?
(define (insert-everywhere/in-all-words source target)
  (cond
    [(empty? target) ...]
    [else ...]))

(define test-word
  (cons (cons 'e (cons 'r empty))
        (cons (cons 'r (cons 'e empty)) empty)))

(check-expect (insert-everywhere/in-all-words 'z (cons (cons 'a empty) (cons 'b empty)))
              (cons (cons 'z (cons 'a (cons 'b empty)))
                    (cons (cons 'a (cons 'z (cons 'b empty)))
                          (cons (cons 'a (cons 'b (cons 'z empty))) empty))))

