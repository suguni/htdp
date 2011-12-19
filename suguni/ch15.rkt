;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ch15

(define-struct parent (children name date eyes))

;; Youngest Generation:
(define Gustav (make-parent empty 'Gustav 1988 'brown))

(define Fred&Eva (list Gustav))

;; Middle Generation:
(define Adam (make-parent empty 'Adam 1950 'yellow))
(define Dave (make-parent empty 'Dave 1955 'black))
(define Eva (make-parent Fred&Eva 'Eva 1965 'blue))
(define Fred (make-parent Fred&Eva 'Fred 1966 'pink))

(define Carl&Bettina (list Adam Dave Eva))

;; Oldest Generation:
(define Carl (make-parent Carl&Bettina 'Carl 1926 'green))
(define Bettina (make-parent Carl&Bettina 'Bettina 1926 'green))

;; blue-eyed-descendent? : parent -> boolean
(define (blue-eyed-descendant? p)
  (cond
    [(symbol=? (parent-eyes p) 'blue) true]
    [else (blue-eyed-children? (parent-children p))]))

(check-expect (blue-eyed-descendant? Gustav) false)
(check-expect (blue-eyed-descendant? Eva) true)
(check-expect (blue-eyed-descendant? Bettina) true)

;; blue-eyed-children? : list-of-children -> boolean
(define (blue-eyed-children? loc)
  (cond
    [(empty? loc) false]
    [else
     (cond
       [(blue-eyed-descendant? (first loc)) true]
       [else (blue-eyed-children? (rest loc))])]))

(check-expect (blue-eyed-children? (list Gustav)) false)
(check-expect (blue-eyed-children? (list Adam Dave Eva)) true)

;; ex 15.1.1
;(blue-eyed-descendant? Eva)
;=>
;(cond
;  [(symbol=? (parent-eyes Eva) 'blue) true]
;  [else (blue-eyed-children? (parent-children Eva))])
;=>
;(cond
;  [true true]
;  [else (blue-eyed-children? (parent-children Eva))])
;=>
;true

;(blue-eyed-descendant? Bettina)
;(cond
;  [(symbol=? (parent-eyes Bettina) 'blue) true]
;  [else (blue-eyed-children? (parent-children Bettina))])
;=>
;(cond
;  [false true]
;  [else (blue-eyed-children? (list Adam Dave Eva))])
;=>
;(blue-eyed-children? (list Adam Dave Eva))
;=>
;(cond
;  [(empty? (list Adam Dave Eva)) false]
;  [else
;   (cond
;     [(blue-eyed-descendant? (first (list Adam Dave Eva))) true]
;     [else (blue-eyed-children? (rest (list Adam Dave Eva)))])])
;=>
;(cond
;  [false false]
;  [else
;   (cond
;     [(blue-eyed-descendant? (first (list Adam Dave Eva))) true]
;     [else (blue-eyed-children? (rest (list Adam Dave Eva)))])])
;=>
;(cond
;  [(blue-eyed-descendant? Adam) true]
;  [else (blue-eyed-children? (list Dave Eva))])
;=>
;(cond
;  [(cond
;     [(symbol=? (parent-eyes Adam) 'blue) true]
;     [else (blue-eyed-children? (parent-children Adam))]) true]
;  [else (blue-eyed-children? (list Dave Eva))])
;=>
;(cond
;  [(cond
;     [false true]
;     [else (blue-eyed-children? empty)]) true]
;  [else (blue-eyed-children? (list Dave Eva))])
;=>
;(cond
;  [false true]
;  [else (blue-eyed-children? (list Dave Eva))])
;=>
;(blue-eyed-children? (list Dave Eva))
;=>
;(cond
;  [(empty? (list Dave Eva)) false]
;  [else
;   (cond
;     [(blue-eyed-descendant? (first (list Dave Eva))) true]
;     [else (blue-eyed-children? (rest (list Dave Eva)))])])
;=>
;(cond
;  [false false]
;  [else
;   (cond
;     [(blue-eyed-descendant? Dave) true]
;     [else (blue-eyed-children? (list Eva))])])
;=>
;(cond
;  [(blue-eyed-descendant? Dave) true]
;  [else (blue-eyed-children? (list Eva))])
;=>
;(cond
;  [(cond
;     [(symbol=? (parent-eyes Dave) 'blue) true]
;     [else (blue-eyed-children? (parent-children Dave))]) true]
;  [else (blue-eyed-children? (list Eva))])
;=>
;(cond
;  [(cond
;     [false true]
;     [else (blue-eyed-children? (parent-children empty))]) true]
;  [else (blue-eyed-children? (list Eva))])
;=>
;(cond
;  [false true]
;  [else (blue-eyed-children? (list Eva))])
;=>
;(blue-eyed-children? (list Eva))
;=>
;(cond
;  [(empty? (list Eva)) false]
;  [else
;   (cond
;     [(blue-eyed-descendant? (first (list Eva))) true]
;     [else (blue-eyed-children? (rest (list Eva)))])])
;=>
;(cond
;  [false false]
;  [else
;   (cond
;     [(blue-eyed-descendant? Eva) true]
;     [else (blue-eyed-children? empty)])])
;=>
;(cond
;  [true true]
;  [else (blue-eyed-children? empty)])
;=>
;true

;; ex 15.1.2
;; how-far-removed : parent -> number or false
(define (how-far-removed p)
  (cond
    [(blue-eyed-descendant? p)
     (cond
       [(symbol=? (parent-eyes p) 'blue) 0]
       [else (+ 1 (apply min (filter number? (how-far-removed-children (parent-children p)))))])]
    [else false]))

;; apply, min, filter...???

;; tests
(check-expect (how-far-removed Gustav) false)
(check-expect (how-far-removed Eva) 0)
(check-expect (how-far-removed Fred) false)
(check-expect (how-far-removed Dave) false)
(check-expect (how-far-removed Adam) false)
(check-expect (how-far-removed Carl) 1)
(check-expect (how-far-removed Bettina) 1)

;; how-far-removed-children : list-of-children -> list-of-number/false
(define (how-far-removed-children loc)
  (cond
    [(empty? loc) empty]
    [else
     (cons (how-far-removed (first loc))
           (how-far-removed-children (rest loc)))]))

;; another test
(define a (make-parent empty 'a 1988 'blue))
(define a1 (make-parent empty 'a1 1989 'black))
(define b (make-parent (list a a1) 'b 1977 'green))
(define b1 (make-parent empty 'b1 1977 'blue))
(define c (make-parent (list b b1) 'c 1967 'red))
(define d (make-parent (list c) 'd 1967 'black))
(check-expect (how-far-removed d) 2)

;; ex 15.1.3
;; count-descendants : parent -> number
(define (count-descendants p)
  (+ 1 (count-descendants-children (parent-children p))))

(define (count-descendants-children lop)
  (cond
    [(empty? lop) 0]
    [else
     (+ (count-descendants (first lop))
        (count-descendants-children (rest lop)))]))

;; tests
(check-expect (count-descendants d) 6)
(check-expect (count-descendants Carl) 5)

;; count-proper-descendants : parent -> number
(define (count-proper-descendants p)
  (count-proper-descendants-children (parent-children p)))

;; ???
(define (count-proper-descendants-children lop)
  (cond
    [(empty? lop) 0]
    [else
     (+ 1
        (count-proper-descendants (first lop))
        (count-proper-descendants-children (rest lop)))]))

;; tests
(check-expect (count-proper-descendants d) 5)
(check-expect (count-proper-descendants Carl) 4)

;; ex 15.1.4
;; eye-colors : parent -> list-of-colors
(define (eye-colors p)
  (cons (parent-eyes p) (eye-colors-children (parent-children p))))

(define (eye-colors-children lop)
  (cond
    [(empty? lop) empty]
    [else
     (append
      (eye-colors (first lop))
      (eye-colors-children (rest lop)))]))

(check-expect (eye-colors d) (list 'black 'red 'green 'blue 'black 'blue))
(check-expect (eye-colors Carl) (list 'green 'yellow 'black 'blue 'brown))

;; ch 15.3

;A Web-page (short: WP) is a structure:
; (make-wp h p) 
;where h is a symbol and p is a (Web) document.
;
;A (Web) document is either
; 1. empty,
; 2. (cons s p)
;    where s is a symbol and p is a document, or
; 3. (cons w p)
;    where w is a Web page and p is a document.

(define-struct wp (header body))

;; ex 15.3.1
;; size : wp -> #symbol(number)
(define (size a-wp)
  (+ (size-body (wp-body a-wp))))

;; size-body : web-document -> #symbol(number)
(define (size-body wd)
  (cond
    [(empty? wd) 1]
    [(symbol? (first wd)) (+ 1 (size-body (rest wd)))]
    [(wp? (first wd)) (+ (size (first wd)) (size-body (rest wd)))]))

;; test
(define wp1 (make-wp 'top-title (list 'hello 'top 'page)))
(define wp2 (make-wp 'sub1-title (list 'hello 'sub1 'page wp1 'hi 'sub1)))
(check-expect (size wp1) 4)
(check-expect (size wp2) 10)

;; ex 15.3.2
;; wp-to-file : wp -> list-of-symbols
(define (wp-to-file a-wp)
  (cons (wp-header a-wp)
        (wp-to-file-body (wp-body a-wp))))

;; wp-to-file-body : document -> list-of-symbols
(define (wp-to-file-body wd)
  (cond
    [(empty? wd) empty]
    [(symbol? (first wd))
     (cons (first wd)
           (wp-to-file-body (rest wd)))]
    [(wp? (first wd))
     (cons (wp-header (first wd))
           (wp-to-file-body (rest wd)))]))

;; test
(check-expect (wp-to-file wp1) (list 'top-title 'hello 'top 'page))
(check-expect (wp-to-file wp2) (list 'sub1-title 'hello 'sub1 'page 'top-title 'hi 'sub1))

;; ex 15.3.3
;; occurs : symbol wp -> boolean
(define (occurs s a-wp)
  (or 
   (symbol=? s (wp-header a-wp))
   (occurs-body s (wp-body a-wp))))

;; occurs-body : symbol document -> boolean
(define (occurs-body s wd)
  (cond
    [(empty? wd) false]
    [(symbol? (first wd)) (or (symbol=? s (first wd))
                              (occurs-body s (rest wd)))]
    [(wp? (first wd)) (or (occurs s (first wd))
                          (occurs-body s (rest wd)))]))

;; test
(check-expect (occurs 'hello wp1) true)
(check-expect (occurs 'hi wp1) false)
(check-expect (occurs 'hi wp2) true)
(check-expect (occurs 'good wp2) false)

;; ex 15.3.4 -- too complex. right???
;; find : symbol wp -> list-of-headers or false
(define (find s a-wp)
  (cond
    [(occurs? s a-wp) (find-body s (wp-header a-wp) (wp-body a-wp))]
    [else false]))

;; find-body : symbol document -> list-of-headers
(define (find-body s h wd)
  (cond
    [(empty? wd) empty]
    [(symbol? (first wd))
     (cond 
       [(symbol=? s (first wd))
        (cons h (find-body s h (next-wp (rest wd))))]
       [else (find-body s h (rest wd))])]
    [(wp? (first wd))
     (cond
       [(occurs? s (first wd)) (append (find s (first wd))
                                       (find-body s h (rest wd)))]
       [else (find-body s h (rest wd))])]))

;; next-wp : document -> document
(define (next-wp wd)
  (cond
    [(empty? wd) empty]
    [(wp? (first wd)) wd]
    [else (next-wp (rest wd))]))

;; test
(check-expect (next-wp (wp-body wp1)) empty)
(check-expect (next-wp (rest (wp-body wp2))) (list wp1 'hi 'sub1))

;; occurs? : symbol wp -> boolean
(define (occurs? s a-wp)
   (occurs-body? s (wp-body a-wp)))

;; occurs-d-body : symbol document -> boolean
(define (occurs-body? s wd)
  (cond
    [(empty? wd) false]
    [(symbol? (first wd)) (or (symbol=? s (first wd))
                              (occurs-body? s (rest wd)))]
    [(wp? (first wd)) (or (occurs? s (first wd))
                          (occurs-body? s (rest wd)))]))

;; tests
(check-expect (find 'hello wp1) (list 'top-title))
(check-expect (find 'page wp1) (list 'top-title))
(check-expect (find 'hi wp1) false)
(check-expect (find 'hi wp2) (list 'sub1-title))
(check-expect (find 'good wp2) false)
(check-expect (find 'page wp2) (list 'sub1-title 'top-title))

