;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter.14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;chatper 14.1
(define-struct child (father mother name date eye))
;; 가계도 항목 family-tree-node(ftn)는 다음 두 가지 중 하나다.
;; 1.empty
;; 2.(make-child f m na da ec)
;; f와 m은 ftn이고, ,na와 ec는 기호, da는 숫자다.

(define Carl (make-child empty empty 'Carl 1926 'green))
(define Bettina (make-child empty empty 'Bettina 1926 'green))

(define Adam (make-child Carl Bettina 'Adam 1950 'yellow))
(define Dave (make-child Carl Bettina 'Dave 1955 'black))
(define Eva (make-child Carl Bettina 'Eva 1965 'blue))
(define Fred (make-child empty empty 'Fred 1966 'pink))

(define Gustav (make-child Fred Eva 'Gustav 1988 'brown))

;;fun-for-ftn: ftn -> ?
;(define (fun-for-ftn a-ftree)
;  (cond
;    [(empty? a-ftree) ...]
;    [else
;     ... (fun-for-ftn (child-father a-ftree)) ...
;     ... (fun-for-ftn (child-mother a-ftree)) ...
;     ... (child-name a-ftree) ...
;     ... (child-date a-ftree) ...
;     ... (child-eyes a-ftree)...]))

;blue-eyed-ancester? : ftn->boolean
; a-ftree에 눈 색깔 필드에 'blue가 있는 child 구조체가 존재하는지 검사
(define (blue-eyed-ancester? a-ftree)
  (cond 
    [(empty? a-ftree) false]
    [else
     (cond
       [(symbol=? 'blue (child-eye a-ftree)) true]
       [(blue-eyed-ancester? (child-father a-ftree)) true]
       [(blue-eyed-ancester? (child-mother a-ftree)) true]
       [else false])]))

;;ex.14.1.1
;(blue-eyed-ancester? Carl)
; = (blue-eyed-ancester? (make-child empty empty 'Carl 1926 'green))
; = (cond [(empty? Carl) false]
;         [else (or (symbol? (child-eyes Carl) 'blue)
;             (or (blue-eyed-ancester? (child-father Carl))
;                 (blue-eyed-ancester? (child-mother Carl))))])
; = (or false (or (blue-eyed-ancester? empty) (blue-eyed-ancester? empty)))
; = false

;;ex.14.1.2

;;ex.14.1.3
(define (count-persons a-ftree)
  (cond
    [(empty? a-ftree) 0]
    [else (+ 1 (count-persons (child-mother a-ftree)) (count-persons (child-father a-ftree)))]))

;;(count-persons Gustav)

;;ex.14.1.4

;;ex.14.1.5
(define (eye-colors a-ftree)
  (cond
    [(empty? a-ftree) empty]
    [else (cons (child-eye a-ftree) (append (list (eye-colors (child-mother a-ftree))) (list (eye-colors (child-father a-ftree)))))]))
;(eye-colors Gustav)

;;ex.14.1.6

;; chapter 14.2
(define-struct node (ssn name left right))
;; 이진 트리(BT)는 다음 두 가지 중 하나다.
;1. false
;2. (make-node soc pn lft rgt)
; soc(주민등록번호)는 수, pn(이름)은 기호, lft(왼쪽 트리)와 rgt(오른쪽 트리)는 이진 트리

;;ex.14.2.1
;; contains-bt : number, a-bt -> boolean
(define (contains-bt n a-bt)
  (cond
    [(false? a-bt) false]
    [(= n (node-ssn a-bt)) true]
    [else (or (contains-bt n (node-left a-bt))
              (contains-bt n (node-right a-bt)))]))

;;ex.14.2.2
(define (search-bt n a-bt)
  (cond
    [(false? a-bt) false]
    [(= n (node-ssn a-bt)) (node-name a-bt)]
    [(contains-bt n (node-left a-bt)) ...]
    [(contains-bt n (node-right a-bt)) ...]
    [else false]))

;ex.14.2.3
(define (inorder a-bt)
  (cond
    [(false? a-bt) false]
    [else (append (inorder (node-left a-bt)) (list (node-name a-bt)) (inorder (node-right a-bt)))]))

;ex.14.2.4
(define (search-bst n a-bst)
  (cond
    [(false? a-bst) false]
    [(= n (node-ssn a-bst)) (node-name a-bst)]
    [(< n (node-ssn a-bst)) (search-bst n (node-left a-bst))]
    [(> n (node-ssn a-bst)) (search-bst n (node-right a-bst))]))
 
;ex.14.2.5
(define (create-bst b n s)
  (cond
    [(false? b) (make-node n s false false)]
    [(< n (node-ssn b)) (make-node (node-ssn b) 
                                   (node-name b)
                                   (create-bst (node-left b) n s)
                                   false)]
    [(> n (node-ssn b)) (make-node (node-ssn b)
                                   (node-name b)
                                   false
                                   (create-bst (node-right b) n s))]))

;ex.14.2.6
;(define (create-bst-from-list a-list name)
;  (cond
;    [(empty? a-list) false]
;    [else
;     (create-bst (create-bst (rest a-list


;chapter 14.3
;웹페이지(WP)는 다음 세 가지 중 하나다.
;1. empty
;2. (cons s wp): s(단어)는기호 wp는 웹페이지 
;3. (cons ewp wp): ewp와 wp는 웹페이지

(define (size a-wp)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp)) (+ 1 (size (rest a-wp)))]
    [else (+ (first a-wp) (rest a-wp))]))

;;ex.14.3.1

;;ex.14.3.2
(define (occur1 a-wp s)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp))
     (cond
       [(symbol=? (first a-wp) s) (+ 1 (occur1 (rest a-wp) s))]
       [else (occur1 (rest a-wp) s)])]
    [else (occur1 (rest a-wp) s)]))

;;chapter 14.4
(define-struct add (left right))
(define-struct mul (left right))