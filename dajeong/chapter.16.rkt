;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chapter.16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;ex.16.2.1
(define Code (list 'hang 'draw))
(define Docs (list 'read!))
(define Libs (list Code Docs))
(define Text (list 'part1 'part2 'part3))
(define TS (list'read! Text Libs))

;ex.16.2.2
(define (how-many a-dir)
 (cond
   [(empty? a-dir) 0]
   [(symbol? (first a-dir)) (+ 1 (how-many (rest a-dir)))]
   [else (+ (how-many (first a-dir)) (how-many (rest a-dir)))]));

(check-expect 7 (how-many TS))

;ex.16.2.3
(define-struct dir1 (name content))
(define-struct dir2 (name content size recognized?))
;디렉토리는 구조체다.
;(make-dir n c s r)
;여기에서 n(이름)은 기호, c(정보)는 파일 및 디렉토리의 리스트, s(사이즈)는 숫자, r(시스템 속성)은 불린이다.

;파일 및 디렉토리 리스트(LOFD, List Of Files and Directories)는 다음 세 가지 중 하나다.
;1.empty
;2.(cons f d): f는 파일이고, d는 파일 및 디렉토리 리스트다.
;3.(cons d1 d2): d1, d2는 파일 및 디렉토리 리스트다.

;ex.16.2.4
(define Code2 (make-dir1 'Code (list 'hang 'draw)))
(define Docs2 (make-dir1 'Docs (list 'read!)))
(define Libs2 (make-dir1 'Libs (list Code2 Docs2)))
(define Text2 (make-dir1 'Text (list 'part1 'part2 'part3)))
(define TS2 (make-dir1 'TS (list Text2 Libs2 'read!)))

;how-many-2: dir(struct) -> number
;디렉토리 내 파일 개수를 구한다.
(define (how-many-2 a-dir)
  (how-many-2-for-list (dir1-content a-dir)))

;how-many-2-for-list: list -> number
(define (how-many-2-for-list lofd)
  (cond
    [(empty? lofd) 0]
    [else 
     (cond
       [(symbol? (first lofd)) (+ 1 (how-many-2-for-list (rest lofd)))]
       [else (+ (how-many-2 (first lofd)) (how-many-2-for-list (rest lofd)))])]))

(check-expect 7 (how-many-2 TS2))