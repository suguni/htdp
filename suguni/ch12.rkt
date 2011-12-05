;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch12) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp") (lib "arrow.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp") (lib "arrow.ss" "teachpack" "htdp")))))
;; ch 12

;; sort : list-of-numbers -> list-of-numbers
;; to create a sorted list of numbers from all the numbers in alon
(define (sort alon)
  (cond
    [(empty? alon) empty]
    [else (insert (first alon) (sort (rest alon)))]))

;; examples & test
(check-expect (sort empty)
              empty)
(check-expect (sort (cons 1297.04 (cons 20000.00 (cons -505.25 empty))))
              (cons 20000.00 (cons 1297.04 (cons -505.25 empty))))

;; insert : number list-of-numbers -> list-of-numbers
;; to create a list of numbers from n and the numbers on alon
;; that is sorted in descending order; alon is already sorted
(define (insert n alon)
  (cond
    [(empty? alon) (cons n empty)]
    [else
     (cond
       [(>= n (first alon)) (cons n alon)]
       [else (cons (first alon) (insert n (rest alon)))])]))

;; examples & test
(check-expect (insert 5 empty)
              (cons 5 empty))
(check-expect (insert 1297.04 (cons 20000.00 (cons -505.25 empty)))
              (cons 20000.00 (cons 1297.04 (cons -505.25 empty))))

;; ex 12.2.1
;; from: string, date: number, message: string
(define-struct mail (from date message))

;; sort-mail-by-date : list-of-mails -> list-of-mails
;; sorts lists of mail message by date
(define (sort-mail-by-date mails)
  (cond
    [(empty? mails) empty]
    [else (insert-mail-by-date (first mails) (sort-mail-by-date (rest mails)))]))

;; test data set
(define sample-mail-list
  (cons (make-mail "suguni" 4 "Hello suguni")
        (cons (make-mail "chorr" 6 "Hello chorr")
              (cons (make-mail "leejun" 2 "Hello leejun")
                    (cons (make-mail "kanhki" 9 "Hello kanhki")
                          (cons (make-mail "ptodvy" 3 "Hello ptodvy")
                                empty))))))

(define sample-mail-list-sorted-by-date 
  (cons (make-mail "kanhki" 9 "Hello kanhki")
        (cons (make-mail "chorr" 6 "Hello chorr")
              (cons (make-mail "suguni" 4 "Hello suguni")
                    (cons (make-mail "ptodvy" 3 "Hello ptodvy")
                          (cons (make-mail "leejun" 2 "Hello leejun")
                                empty))))))

;; test
(check-expect (sort-mail-by-date empty)
               empty)

(check-expect (sort-mail-by-date sample-mail-list)
              sample-mail-list-sorted-by-date)

;; insert-mail-by-date
(define (insert-mail-by-date m mails)
  (cond
    [(empty? mails) (cons m empty)]
    [else
     (cond
       [(>= (mail-date m) (mail-date (first mails))) (cons m mails)]
       [else (cons (first mails) (insert-mail-by-date m (rest mails)))])]))

(define sample-mail (make-mail "headzero" 5 "Hello headzero"))
(define insert-mail-by-date-result
  (cons (make-mail "kanhki" 9 "Hello kanhki")
        (cons (make-mail "chorr" 6 "Hello chorr")
              (cons (make-mail "headzero" 5 "Hello headzero")
                    (cons (make-mail "suguni" 4 "Hello suguni")
                          (cons (make-mail "ptodvy" 3 "Hello ptodvy")
                                (cons (make-mail "leejun" 2 "Hello leejun")
                                      empty)))))))

;; test
(check-expect (insert-mail-by-date sample-mail empty)
              (cons sample-mail empty))

(check-expect (insert-mail-by-date sample-mail sample-mail-list-sorted-by-date)
              insert-mail-by-date-result)

;; sort-mail-by-from : list-of-mails -> list-of-mails
;; sorts lists of mail message by from
(define (sort-mail-by-from mails)
  (cond
    [(empty? mails) empty]
    [else (insert-mail-by-from (first mails) (sort-mail-by-from (rest mails)))]))

;; test data set
(define sample-mail-list-sorted-by-from
  (cons (make-mail "chorr" 6 "Hello chorr")
        (cons (make-mail "kanhki" 9 "Hello kanhki")
              (cons (make-mail "leejun" 2 "Hello leejun")
                    (cons (make-mail "ptodvy" 3 "Hello ptodvy")
                          (cons (make-mail "suguni" 4 "Hello suguni")
                                empty))))))

;; test
(check-expect (sort-mail-by-from empty)
               empty)

(check-expect (sort-mail-by-from sample-mail-list)
              sample-mail-list-sorted-by-from)

;; insert-mail-by-from : mail list-of-mails -> list-of-mails
(define (insert-mail-by-from m mails)
  (cond
    [(empty? mails) (cons m empty)]
    [else
     (cond
       [(string<? (mail-from m) (mail-from (first mails))) (cons m mails)]
       [else (cons (first mails) (insert-mail-by-from m (rest mails)))])]))

;; test data set
(define insert-mail-by-from-result
  (cons (make-mail "chorr" 6 "Hello chorr")
        (cons (make-mail "headzero" 5 "Hello headzero")
              (cons (make-mail "kanhki" 9 "Hello kanhki")
                    (cons (make-mail "leejun" 2 "Hello leejun")
                          (cons (make-mail "ptodvy" 3 "Hello ptodvy")
                                (cons (make-mail "suguni" 4 "Hello suguni")
                                      empty)))))))

;; test
(check-expect (insert-mail-by-date sample-mail empty)
              (cons sample-mail empty))

(check-expect (insert-mail-by-from sample-mail sample-mail-list-sorted-by-from)
              insert-mail-by-from-result)

;; ex 12.2.2

;; search : number list-of-numbers  ->  boolean
(define (search n alon)
  (cond
    [(empty? alon) false]
    [else (or (= (first alon) n) (search n (rest alon)))]))

;; search-sorted : number list-of-numbers -> boolean
;; list-of-numbers must be sorted.
(define (search-sorted n alon)
  (cond
    [(or (empty? alon)
         (< (first alon) n)) false]
    [else
     (or (= (first alon) n) (search-sorted n (rest alon)))]))

;; test
(check-expect (search-sorted 10 empty) false)
(check-expect (search-sorted 10 (cons 20 (cons 14 (cons 10 (cons 6 empty))))) true)
(check-expect (search-sorted 10 (cons 20 (cons 15 (cons 9 (cons 6 empty))))) false)

;; ch 12.3

;; polygon - triangle
(define sample-triangle (cons (make-posn 10 10)
                              (cons (make-posn 60 60)
                                    (cons (make-posn 10 60)
                                          empty))))

;; draw-polygon : polygon -> true => rename connect-dots
;; to draw the polygon specified by a-poly
(define (connect-dots a-poly)
  (cond
    [(empty? (rest a-poly)) true]
    [else (and (draw-solid-line (first a-poly) (second a-poly))
               (connect-dots (rest a-poly)))]))

;; last : polygon -> posn
;; to extract the last posn on a-poly
(define (last a-poly)
  (cond
    [(empty? (rest a-poly)) (first a-poly)]
    [else (last (rest a-poly))]))

;; test
(check-expect (last sample-triangle) (make-posn 10 60))

;; draw-polygon : polygon -> true
(define (draw-polygon a-poly)
  (connect-dots (cons (last a-poly) a-poly)))

;; (start 100 100)
;; (draw-polygon sample-triangle)

;; ex 12.3.1

;; add-at-end : posn polygon -> polygon
(define (add-at-end p a-poly)
  (cond
    [(empty? (rest a-poly)) (cons (first a-poly) (cons p empty))]
    [else
     (cons (first a-poly)
           (add-at-end p (rest a-poly)))]))

(check-expect (add-at-end (first sample-triangle) sample-triangle)
              (cons (make-posn 10 10)
                    (cons (make-posn 60 60)
                          (cons (make-posn 10 60)
                                (cons (make-posn 10 10)
                                      empty)))))

(define (draw-polygon-2 a-poly)
  (connect-dots (add-at-end (first a-poly) a-poly)))

; (start 100 100)
; (draw-polygon-2 sample-triangle)

;; ex 12.3.2
;; connect-dots-3 : posn polygon -> true
;; A posn is first posn of polygon
(define (connect-dots-3 a-posn a-poly)
  (cond
    [(empty? (rest a-poly)) (draw-solid-line (first a-poly) a-posn)]
    [else (and (draw-solid-line (first a-poly) (second a-poly))
               (connect-dots-3 a-posn (rest a-poly)))]))

;; draw-polygon-3 : a-poly -> true
;; use connect-dots-3
(define (draw-polygon-3 a-poly)
  (connect-dots-3 (first a-poly) a-poly))

;; test
; (start 100 100)
; (draw-polygon-3 sample-triangle)

;; ex 12.4.1

; A list-of-words is either
;    (cons w empty) where w is a word, or
;    (cons w low) where w is a word structure and low is a list-of-words.

;; examples
;; word
;(cons 'c (cons 'a (cons 't empty))) ; => cat
;; list-of-words
;(cons (cons 'c (cons 'a (cons 't empty))) ;; cat
;      (cons (cons 'c (cons 't (cons 'a empty))) ;; cta
;            (cons (cons 'a (cons 'c (cons 't empty))) ;; act
;                  (cons (cons 'a (cons 't (cons 'c empty))) ;; atc
;                        (cons (cons 't (cons 'a (cons 'c empty))) ;; tac
;                              (cons (cons 't (cons 'c (cons 'a empty))) ;; tca
;                                    empty))))))
;; ~= (cons "cat" (cons "cta" (cons "act" (cons "atc" (cons "tac" (cons "tca" empty))))))

;; arrangements : word -> list-of-words
;; to create a list of all rearrangements of the letters in a-word
(define (arrangements a-word)
  (cond
    [(empty? a-word) (cons empty empty)]
    [else
     (insert-everywhere/in-all-words (first a-word)
                                     (arrangements (rest a-word)))]))

;; insert-everywhere/in-all-words : symbol list-of-words -> list-of-words
(define (insert-everywhere/in-all-words c low)
  (cond
    [(empty? low) empty]
    [else (append (insert-everywhere/in-a-word c (first low))
                  (insert-everywhere/in-all-words c (rest low)))]))

;; test
(check-expect (insert-everywhere/in-all-words 'd empty)
              empty)

(check-expect (insert-everywhere/in-all-words
               'd (list (cons 'e empty)))
              (list (cons 'd (cons 'e empty))   ; de
                    (cons 'e (cons 'd empty)))) ; ed
                          
(check-expect (insert-everywhere/in-all-words
               'd (list (cons 'e (cons 'r empty))
                        (cons 'r (cons 'e empty))))
              (list (cons 'd (cons 'e (cons 'r empty)))   ; der
                    (cons 'e (cons 'd (cons 'r empty)))   ; edr
                    (cons 'e (cons 'r (cons 'd empty)))   ; erd
                    (cons 'd (cons 'r (cons 'e empty)))   ; dre
                    (cons 'r (cons 'd (cons 'e empty)))   ; rde
                    (cons 'r (cons 'e (cons 'd empty))))) ; red

;; insert-everywhere/in-a-word : symbol word -> list-of-words
(define (insert-everywhere/in-a-word c word)
  (cond
    [(empty? word) (list (cons c empty))]
    [else
     (cons (append (list c (first word)) (rest word))
           (insert-first/in-all-words (first word) (insert-everywhere/in-a-word c (rest word))))]))

;; test
(check-expect (insert-everywhere/in-a-word 'd empty)
              (cons (cons 'd empty) empty))

(check-expect (insert-everywhere/in-a-word 'd (cons 'e empty))
              (list (cons 'd (cons 'e empty))   ; de
                    (cons 'e (cons 'd empty)))) ; ed

(check-expect (insert-everywhere/in-a-word 'd (cons 'e (cons 'r empty)))
              (list (cons 'd (cons 'e (cons 'r empty)))   ; der
                    (cons 'e (cons 'd (cons 'r empty)))   ; edr
                    (cons 'e (cons 'r (cons 'd empty))))) ; erd

;; insert-first/in-all-words : symbol list-of-words -> list-of-words
(define (insert-first/in-all-words c low)
  (cond
    [(empty? low) empty]
    [else
     (cons (cons c (first low))
           (insert-first/in-all-words c (rest low)))]))

;; test
(check-expect (insert-first/in-all-words 'd (list (cons 'e (cons 'r empty))
                                                  (cons 'r (cons 'e empty))))
              (list (cons 'd (cons 'e (cons 'r empty)))
                    (cons 'd (cons 'r (cons 'e empty)))))

; Note!!!
;   insert-everywhere/in-all-words 에서는 empty? cond clause가 empty를 반환하는데,
;   insert-everywhere/in-a-word 에서의 empty? cond clause에서는 list를 반환한다.
; 왜???
;   insert-everywhere/in-all-words 에서 empty?라는 것은 word가 없다는 것을 의미하므로,
;   더이상 처리할 것이 없음을 의미하므로 empty를 반환
;   insert-everywhere/in-a-word에서는 empty?라고 하더라도 empty 자체가 word 이므로,
;   list-of-words로 만들어 반환한다.
