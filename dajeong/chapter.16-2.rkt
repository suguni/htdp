;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chapter.16-2) (read-case-sensitive #t) (teachpacks ((lib "dir.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.ss" "teachpack" "htdp")))))
;16.3 함수 및 프로그램 개선하기
;(define-struct file (name size content))
;(make-file n s c)
;n(이름)은 기호, s(크기)는 수, c는 임의의 scheme값이다.
;(define-struct dir (name dirs files))
;(make-dir n ds fs)
;n(이름)은 기호, ds는 디렉토리 리스트, fs는 파일 리스트다.

;디렉토리 리스트는 다음 두가지 중 하나다.
;1. empty
;2. (cons s lod): s는 디렉토리이고, lod는 디렉토리 리스트다.

;파일 리스트는 다음 두가지 중 하나다.
;1. empty
;2. (cons s lof): s는 파일이고, lof는 파일 리스트다.

(define hang (make-file 'hang 8 empty))
(define draw (make-file 'draw 2 empty))
(define read! (make-file 'read! 19 empty))
(define read!2 (make-file 'read! 10 empty))
(define part1 (make-file 'part1 99 empty))
(define part2 (make-file 'part2 52 empty))
(define part3 (make-file 'part3 17 empty))
(define Code (make-dir 'Code empty (cons hang (cons draw empty))))
(define Docs (make-dir 'Docs empty (cons read! empty)))
(define Text (make-dir 'Text empty (cons part1 (cons part2 (cons part3 empty)))))
(define Libs (make-dir 'Libs (cons Code (cons Docs empty)) empty))
(define TS (make-dir 'TS (cons Text (cons Libs empty)) (cons read!2 empty)))
;ex.16.3.1