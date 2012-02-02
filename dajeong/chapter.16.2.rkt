;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chapter.16.2) (read-case-sensitive #t) (teachpacks ((lib "dir.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.ss" "teachpack" "htdp")))))
;; 모델3
(define-struct dir (name dirs files))
(define-struct file (name size content))

;파일은 구조체다.
;(make-file n s x)
; 여기에서 n(이름)은 기호, s(크기)는 수, x(내용)는 임의의 Scheme 값이다.

;디렉토리는 구조체다.
;(make-dir n d f)
; 여기에서 n(이름)은 기호, d는 하위 디렉토리 리스트이고, f는 파일 리스트이다. 

; 파일 리스트는 다음 두가지 중 하나다.
;1.empty
;2.(cons s lof): s는 파일이고, lof는 파일 리스트다.

;디렉토리 리스트는 다음 세가지 중 하나다.
;1.empty
;2.(cons s lod): s는 디렉토리이고, lod는 디렉토리 리스트다.

