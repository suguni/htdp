;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ch22) (read-case-sensitive #t) (teachpacks ((lib "gui.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "gui.ss" "teachpack" "htdp")))))
;; filter2: (X Y -> boolean) -> ((listof X) Y -> (listof X))
(define (filter2 rel-op)
  (local ((define (abs-fun alon t)
            (cond
              [(empty? alon) empty]
              [else
               (cond
                 [(rel-op (first alon) t)
                  (cons (first alon)
                        (abs-fun (rest alon) t))]
                 [else
                  (abs-fun (rest alon) t)])])))
    abs-fun))

;; ex 22.2.1
;; C->F : number -> number
;; (C * 9/5) + 32 = F
(define (C->F n)
  (+ (* n 9/5) 32))

;; inventory structure
(define-struct IR (name price))

;; map2 : (X -> Y) -> ((listof X) -> (listof Y))
(define (map2 op)
  (local ((define (con-fun alon)
            (cond
              [(empty? alon) empty]
              [else
               (cons (op (first alon))
                     (con-fun (rest alon)))])))
    con-fun))

;; convertCF2 : lon -> lon
(define convertCF2 (map2 C->F))

;; names2 : loIR -> los
(define names2 (map2 IR-name))
                   
;; test
(check-expect (convertCF2 (list 10 20 30)) (list 50 68 86))
(check-expect (names2 (list (make-IR 'robot 10) (make-IR 'doll 2)))
              (list 'robot 'doll))

;; ex 22.2.2
;; sort-a : (n n -> boolean) -> ((listof n) -> (listof n))
(define (sort-a op)
  (local ((define (sort alon)
            (cond
              [(empty? alon) empty]
              [else (insert (first alon) (sort (rest alon)))]))
          (define (insert an alon)
            (cond
              [(empty? alon) (list an)]
              [else (cond
                      [(op an (first alon)) (cons an alon)]
                      [else (cons (first alon)
                                  (insert an (rest alon)))])])))
    sort))

(define sort-asc (sort-a <))
(define sort-des (sort-a >))

;; test
(check-expect (sort-asc (list 2 3 1 5 4)) (list 1 2 3 4 5))
(check-expect (sort-des (list 2 3 1 5 4)) (list 5 4 3 2 1))


;; ex 22.2.3
;; fold : (X Y -> Y) Y -> ((listof X) -> Y)
(define (fold op seed)
  (local ((define (f alon)
            (cond
              [(empty? alon) seed]
              [else
               (op (first alon)
                   (f (rest alon)))])))
    f))

(define sum (fold + 0))
(define product (fold * 1))

;; tests
(check-expect (sum (list 1 2 3)) 6)
(check-expect (product (list 1 2 3)) 6)

;; 22.3
;; p 392 - error
;; (define w (create-window (list (list (make-button "Close" (lambda (e) (hide-window w)))))))

(define a-text-field (make-text "Enter Text:"))
(define a-message (make-message "'Hello World' is a silly program."))
(define (echo-message e)
  (draw-message a-message (text-contents a-text-field)))
;(create-window (list (list a-text-field a-message)
;                     (list (make-button "Copy Now" echo-message))))

