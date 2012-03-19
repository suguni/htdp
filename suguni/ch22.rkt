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


;; ex 22.3.1

;; from ex 9.5.5
;; convert : list-of-numbers -> number
(define (convert lon)
  (cond
    [(empty? lon) 0]
    [else
     (+ (first lon)
        (* 10 (convert (rest lon))))]))
'ex-9.5.5-convert-test
(check-expect (convert (cons 1 (cons 2 (cons 3 (cons 4 empty))))) 4321)

;; check-guess-for-list : list-of-numbers, number -> symbol
;; return symbol is one of 'TooSmall, 'Perfect, 'TooLarge
(define (check-guess-for-list guesses target)
  (cond
    [(< (convert guesses) target) 'TooSmall]
    [(= (convert guesses) target) 'Perfect]
    [(> (convert guesses) target) 'TooLarge]))

;; from ex 11.3.1
(define (random-n-m n m)
  (+ (random (- m n)) n))
;; n 이상 m 미만에 있는 임의의 자연수를 출력한다.

;; Model:
;; build-number : (listof digit) -> number
;; ex) (build-number (list 1 2 3)) = 123
(define (build-number x)
  (local ((define (p a b) (+ a (* 10 b))))
    (foldl p 0 x)))

;; tests
(check-expect (build-number (list 1 2 3)) 123)

;; 자리수
;; (define DIGIT-NUM 5)

;; 정답
;; (define answer (random (expt 10 DIGIT-NUM)))

(define (check-guess guess target)
  (cond
    [(> guess target) "Too Large"]
    [(< guess target) "Too Small"]
    [else "Perfect"]))

;; View:
(define DIGITS (build-list 10 number->string))

;(define digit-choosers
;  (local ((define (builder i) (make-choice DIGITS)))
;    (build-list DIGIT-NUM builder)))

;(define a-msg
;  (make-message "Welcome"))

;; Controller:
;(define (check-call-back b)
;  (draw-message a-msg
;                (check-guess
;                 (build-number (map choice-index digit-choosers))
;                 answer)))

;; game start
(define (start-game n)
  (local ((define answer (random (expt 10 n)))
          (define a-msg
            (make-message "Welcome"))
          (define digit-choosers
            (local ((define (builder i) (make-choice DIGITS)))
              (build-list n builder)))
          (define (check-call-back b)
            (draw-message a-msg
                          (check-guess
                           (build-number (map choice-index digit-choosers))
                           answer))))
    (create-window
     (list
      (append digit-choosers (list a-msg))
      (list (make-button "Check Guess" check-call-back))))))
    
;; (start-game 3)

;; ex 22.3.2

(define-struct phone (name number))

(define DB (list (make-phone "steve" 01042062688)
                 (make-phone "bizen" 01048470337)))

(define (assf p? seq)
  (cond
    [(empty? seq) false]
    [(p? (first seq)) (first seq)]
    [else (assf p? (rest seq))]))

(define (start-search-phone i)
  (local ((define (search-callback b)
            (local ((define name (text-contents a-text))
                    (define pno (string->number name)))
              (draw-message a-msg
                            (cond
                              [(number? pno) (search-db-number pno)]
                              [else (number->string (search-db-name name))]))))
          (define (search-db-name k)
            (local ((define (p? p) (string=? (phone-name p) k))
                    (define r (assf p? DB)))
              (cond
                [(phone? r) (phone-number r)]
                [else "Not Found"])))
          (define (search-db-number k)
            (local ((define (p? p) (= (phone-number p) k))
                    (define r (assf p? DB)))
              (cond
                [(phone? r) (phone-name r)]
                [else "Not Found"])))
          (define a-text (make-text "Name: "))
          (define a-msg (make-message "000-0000-0000"))
          (define a-btn (make-button "Search" search-callback)))
    (create-window
     (list (list a-text a-btn)
           (list (make-message "Number:") a-msg)))))

;; ex 22.3.3

(define (cell->string c)
  (cond
    [(number? c) (number->string c)]
    [(symbol? c) (symbol->string c)]))

(define (pad->gui title gui-table)
  (local ((define (cb msg)
             (draw-message a-msg msg))
          (define a-msg (make-message "N")))
    (append
     (list
      (list (make-message title))
      (list a-msg))
     (map (lambda (seq)
            (map (lambda (cell)
                   (let ((label (cell->string cell)))
                     (make-button label
                                  (lambda (e) (cb label)))))
                 seq)) gui-table))))
  
;; tests
(define pad
  '((1 2 3)
    (4 5 6)
    (7 8 9)
    (\# 0 *)))

(define pad2
  '((1 2 3 +)
    (4 5 6 -)
    (7 8 9 *)
    (0 = \. /)))

;; 테스트 불가
;(check-expect (pad->gui "Virtual Phone" pad)
;              (list (list (make-message "Virtual Phone"))
;                    (list (make-message "N"))
;                    (list (make-button "1" cb) (make-button "2" cb) (make-button "3" cb))
;                    (list (make-button "4" cb) (make-button "5" cb) (make-button "6" cb))
;                    (list (make-button "7" cb) (make-button "8" cb) (make-button "9" cb))
;                    (list (make-button "#" cb) (make-button "0" cb) (make-button "*" cb))))
;
;(check-expect (pad->gui "Calculator" pad2)
;              (list (list (make-message "Calculator"))
;                    (list (make-message "N"))
;                    (list (make-button "1" cb) (make-button "2" cb) (make-button "3" cb) (make-button "+" cb))
;                    (list (make-button "4" cb) (make-button "5" cb) (make-button "6" cb) (make-button "-" cb))
;                    (list (make-button "7" cb) (make-button "8" cb) (make-button "9" cb) (make-button "*" cb))
;                    (list (make-button "0" cb) (make-button "=" cb) (make-button "." cb) (make-button "/" cb))))

;; create window
; (create-window (pad->gui "Virtual Phone" pad))
; (create-window (pad->gui "Calculator" pad2))
