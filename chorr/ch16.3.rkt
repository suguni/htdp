;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch16.3) (read-case-sensitive #t) (teachpacks ((lib "dir.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.ss" "teachpack" "htdp")))))


;; ex 16.3.1

;; create-dir : string  ->  dir
;; to create a representation of the directory that a-path specifies:
;; 1. Windows: (create-dir "c:\\windows") 
;; 2. Mac: (create-dir "My Disk:") 
;; 3. Unix: (create-dir "/home/scheme/")
;
;(define (create-dir a-path) ...)

(define TS3
  (make-dir 'TS
            (list (make-dir 'Text empty (list (make-file 'part1 99 empty)
                                              (make-file 'part2 52 empty)
                                              (make-file 'part3 17 empty)))
                  (make-dir 'Libs 
                            (list (make-dir 'Code empty (list (make-file 'hang 8 empty)
                                                              (make-file 'draw 2 empty)))
                                  (make-dir 'Docs empty (list (make-file 'read! 19 empty))))
                            empty))
            (list (make-file 'read! 10 empty))))

;(create-dir ".")


;; ex 16.3.2
;; how-many : list-of-directories -> number
(define (how-many dt)
  (+ (how-many-dir (dir-dirs dt))
     (how-many-file (dir-files dt))))

(define (how-many-dir d)
  ... )

(define (how-many-file f)
  ... )



