;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; ex 16.1.1
;read! 파일은 총 2개
;파일크기는 총 207
;깊이는 4단계


;; ex 16.2.1
;A file is a symbol.
;
;A directory (short: dir) is either
;1. empty;
;2. (cons f d) where f is a file and d is a dir; or
;3. (cons d1 d2) where d1 and d2 are dirs.

(define TS (cons (cons 'read!
                 (cons 'part1 (cons 'part2 (cons 'part3 empty))))
                 (cons (cons 'hang (cons 'draw empty)) (cons 'read! empty))))


;; ex 16.2.2
;; how-many : dir-tree -> number
(define (how-many dt)
  (cond
    [(empty? dt) 0]
    [(symbol? (first dt)) (+ 1 (how-many (rest dt)))]
    [else (+ (how-many (first dt)) (how-many (rest dt)))]))

(check-expect (how-many TS) 7)


(define-struct dir (name content))


;; ex 16.2.3
(define-struct dir-ext (name content size property))


;; ex 16.2.4
(define TS2 
  (make-dir 'TS (list (make-dir 'Text (list 'part1 'part2 'part3))
                      (make-dir 'Libs (list (make-dir 'Code (list 'hang 'draw))
                                            (make-dir 'Docs (list 'read!))))
                      'read!)))


;; ex 16.2.5
;; how-many-2 : lofd -> number
(define (how-many-2 dt)
  (how-many-content (dir-content dt)))

;; how-many-content : dir-content -> number
(define (how-many-content ct)
  (cond
    [(empty? ct) 0]
    [(symbol? (first ct)) (+ 1 (how-many-content (rest ct)))]
    [else (+ (how-many-2 (first ct)) 
             (how-many-content (rest ct)))]))

(check-expect (how-many-2 TS2) 7)



