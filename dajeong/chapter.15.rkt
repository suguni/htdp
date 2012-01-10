;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chpater.15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;chapter 15.1
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

;; blue-eyed-descendant? : parent -> boolean
;; a-parent나 그 자손들 중에 eyes 필드가 'blue 인 항목이 존재하는지 검사한다.
(define (blue-eyed-descendant? a-parent)
  (cond
    [(symbol=? 'blue (parent-eyes a-parent)) true]
    [else (blue-eyed-children? (parent-children a-parent))]))
           
;; blue-eyed-children : list-of-children -> boolean
;; aloc 내의 구조체 중에 눈이 파랗거나 눈이 파란 자손을 둔 항목이 있는지 검사한다.
(define (blue-eyed-children? aloc)
  (cond
    [(empty? aloc) false]
    [else
     (cond
       [(blue-eyed-descendant? (first aloc)) true]
       [else (blue-eyed-children? (rest aloc))])]))

;; 테스트
(not (blue-eyed-children? (list Gustav)))
(blue-eyed-children? (list Adam Dave Eva))

;;ex.15.1.1

;(blue-eyed-descendant? Eva)
;= (blue-eyed-descendant? (make-parent Fred&Eva 'Eva 1965 'blue))
;=(cond
;   [(symbol=? 'blue (parent-eyes a-parent)) true]
;   [else (blue-eyed-children? (parent-eyes a-parent))])
;= true

;(blue-eyed-descendant? Bettina)
;= (blue-eyed-descendant? (make-parent (list Adam Dave Eva) 'Bettina 1926 'green))
;=(cond
;   [(symbol=? 'blue (parent-eyes a-parent)) true]
;   [else (blue-eyed-children? (parent-eyes a-parent))])
;= (blue-eyed-children? (list Adam Dave Eva))
;= (cond
;    [(empty? aloc) false]
;    [else
;     (cond
;       [(blue-eyed-descendant? (first Adam)) true]
;       [else (blue-eyed-children? (list Dave Eva))])])
;= ...

;ex.15.1.2
; how-far-removed : a-parent -> number (false)
;(define (how-far-removed a-parent)
;  (cond
;    [(symbol=? 'blue (parent-eyes a-parent)) 0]
;    [(blue-eyed-children? (parent-children a-parent)) 1]
;    [else 
     
;ex.15.1.3
; count-descendants : a-parent -> number
(define (count-descendants a-parent)
  (cond
    [(empty? (parent-children a-parent)) 0]
    [else (+ 1 (count-children (parent-children a-parent)))]))
;; count-children? list-of-children -> number
(define (count-children loc)
  (cond
    [(empty? loc) 0]
    [else (+ 1 (count-descendants (first loc)) (count-children (rest loc)))]))

;ex.15.1.4
; eye-colors : a-parent -> list
(define (eye-colors a-parent)
  (cond
    [(empty? (parent-children a-parent)) (list (parent-eyes a-parent))]
    [else (append (list (parent-eyes a-parent)) (eye-colors-children (parent-children a-parent)))]))
;eye-colors-children : loc -> list
(define (eye-colors-children loc)
  (cond
    [(empty? loc) empty]
    [else (append (eye-colors (first loc)) (eye-colors-children (rest loc)))]))

;;chapter 15.2
;; 데이터 분석과 디자인
;; 부모(parent)는 구조체이다.
;;(make-parent loc n d e)
;; loc 는 자식 리스트이고, n(이름)과 e(눈 색깔)는 기호이며, d(생일)은 숫자다.
;; 자식 리스트(list-of-children)는 다음 두 가지 중 하나다.
;;1.empty
;;2.(cons p loc): p는 부모이고  lco는 자식 리스트다.

;; 계약, 목적, 헤더

;; 템플릿
;(define (fun-parent a-parent)
;  ... (parent-name a-parent) ...
;  ... (parent-date a-parent) ...
;  ... (parent-eyes a-parent) ...
;  ... (fun-children (parent-children a-parent)) ...)

;(define (fun-children loc)
;  (cond
;    [(empty? loc) ...]
;    [else ... (fun-parent (first loc)) ... (fun-children (rest loc)) ...]))

;; 구현부


;chapter.15.3
(define-struct wp (header body))
;; 웹페이지(WP)는 구조체다.
;; (make-wp h p)
;; 여기에서 h는 기호이고, p는 웹 문서다.
;; 웹 문서는 다음 세 가지 중 하나다.
;; 1. empty
;; 2. (cons s p): s는 기호이고, p는 웹 문서다.
;; 3. (cons w p): w는 웹 페이지이고, p는 웹 문서다.
;; ex.15.3.1

