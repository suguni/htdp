;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ch 16

;; ex 16.1.1
;; 2, 3, 207

;; A file is a symbol.
;; A directory (short: dir) is either
;;     empty;
;;     (cons f d) where f is a file and d is a dir; or
;;     (cons d1 d2) where d1 and d2 are dirs.

;; ex 16.2.1
(define model1-TS '(((part1 part2 part3) ((hang draw) (read!))) read!))

;; ex 16.2.2
;; how-many : model1-dir -> number
(define (how-many-1 dir)
  (cond
    [(empty? dir) 0]
    [(symbol? (first dir)) (add1 (how-many-1 (rest dir)))]
    [else (+ (how-many-1 (first dir)) (how-many-1 (rest dir)))]))

;; test
(check-expect (how-many-1 model1-TS) 7)

(define-struct dir2 (name content))
;; A directory (short: dir) is a structure:
;;   (make-dir n c) 
;; where n is a symbol and c is a list of files and directories.
;; A list-of-files-and-directories (short: LOFD) is either
;;    empty;
;;    (cons f d) where f is a file and d is a LOFD; or
;;    (cons d1 d2) where d1 is a dir and d2 is a LOFD.

;; ex 16.2.3
;; (define-struct dir (name size attr content))

;; ex 16.2.4
(define model2-TS
  (make-dir2 'TS (list
                 (make-dir2 'Text '(part1 part2 part3))
                 (make-dir2 'Libs (list (make-dir2 'Code '(hang draw)) (make-dir2 'Docs '(read!))))
                 'read)))

;; ex 16.2.5
;; how-many-2 : model2-dir -> number
(define (how-many-2 d)
  (how-many-2-content (dir2-content d)))

;; how-many-2-content : dir-content -> number
(define (how-many-2-content content)
  (cond
    [(empty? content) 0]
    [(symbol? (first content)) (add1 (how-many-2-content (rest content)))]
    [else (+ (how-many-2 (first content))
             (how-many-2-content (rest content)))]))

(check-expect (how-many-2 model2-TS) 7)

;; model 3
(define-struct file (name size content))
(define-struct dir (name dirs files))

;; ex 16.3.1
(define model3-TS
  (make-dir 'TS
            (list (make-dir 'Text
                            empty
                            (list (make-file 'part1 99 empty)
                                  (make-file 'part2 52 empty)
                                  (make-file 'part3 17 empty)))
                  (make-dir 'Libs 
                            (list (make-dir 'Code empty (list (make-file 'hang 8 empty)
                                                              (make-file 'draw 2 empty)))
                                  (make-dir 'Docs empty (list (make-file 'read! 19 empty))))
                            empty))
            (list (make-file 'read! 10 empty))))

;; ex 16.3.2
;; how-many : model3-dir -> number
(define (how-many d)
  (+ (how-many-dirs (dir-dirs d))
     (how-many-files (dir-files d))))
  
(define (how-many-files files)
  (cond
    [(empty? files) 0]
    [else (add1 (how-many-files (rest files)))]))

(define (how-many-dirs dirs)
  (cond
    [(empty? dirs) 0]
    [else (+ (how-many (first dirs))
             (how-many-dirs (rest dirs)))]))
  
(check-expect (how-many model3-TS) 7)

;; Why are we confident that the function produces correct results? => I don't know.

;; ex 16.3.3
;; du-dir : dir -> number
(define (du-dir d)
  (+ (du-dir-dirs (dir-dirs d))
     (du-dir-files (dir-files d))))

;; du-dir-dirs : dirs -> number
(define (du-dir-dirs dirs)
  (cond
    [(empty? dirs) 0]
    [else (+ (du-dir (first dirs))
             (du-dir-dirs (rest dirs)))]))
     
;; du-dir-files : files -> number
(define (du-dir-files files)
  (cond
    [(empty? files) 0]
    [else (+ (file-size (first files))
             (du-dir-files (rest files)))]))

;; test
(check-expect (du-dir model3-TS) 207)

;; 디렉터리 and file의 크기를 1로 하여 용량 계산
;; du-dir2 : dir -> number
(define (du-dir2 d)
  (+ (du-dir2-dirs (dir-dirs d))
     (du-dir2-files (dir-files d))))

;; du-dir-dirs : dirs -> number
(define (du-dir2-dirs dirs)
  (cond
    [(empty? dirs) 0]
    [else (+ 1
             (du-dir2 (first dirs))
             (du-dir2-dirs (rest dirs)))]))
     
;; du-dir-files : files -> number
(define (du-dir2-files files)
  (cond
    [(empty? files) 0]
    [else (+ 1
             (file-size (first files))
             (du-dir2-files (rest files)))]))

;; test
(check-expect (du-dir2 model3-TS) 218)

;; ex 16.3.4
;; find? : dir symbol -> boolean
(define (find? d fn)
  (or (find-dirs? (dir-dirs d) fn)
      (find-files? (dir-files d) fn)))

;; find-dirs? : list-of-dirs symbol -> boolean
(define (find-dirs? lod fn)
  (cond
    [(empty? lod) false]
    [else (or (find? (first lod) fn)
              (find-dirs? (rest lod) fn))]))

;; find-files? : list-of-files symbol -> boolean
(define (find-files? lof fn)
  (cond
    [(empty? lof) false]
    [(symbol=? (file-name (first lof)) fn) true]
    [else (find-files? (rest lof) fn)]))

;; test
(check-expect (find? model3-TS 'part2) true)
(check-expect (find? model3-TS 'part4) false)
(check-expect (find? model3-TS 'draw) true)

;; Challenge
;; find : dir symbol -> list-of-symbols
(define (find d fn)
  (cond
    [(find? d fn) (find-dir d fn)]
    [else false]))

(define (find-dir d fn)
  (cond
    [(find-files? (dir-files d) fn) (cons (dir-name d) empty)]
    [else (cons (dir-name d) (find-dirs (dir-dirs d) fn))]))

(define (find-dirs dirs fn)
  (cond
    [(empty? dirs) empty]
    [(find? (first dirs) fn) (find-dir (first dirs) fn)]
    [else (find-dirs (rest dirs) fn)]))

(check-expect (find model3-TS 'part3) (list 'TS 'Text))
(check-expect (find model3-TS 'read!) (list 'TS))
(check-expect (find model3-TS 'hang) (list 'TS 'Libs 'Code))

;; find2 : dir symbol -> list-of-LOS
;(define (find2 d fn)
;  (cond
;    [(find? d fn) (cons (find2-dir d fn) (find2-dirs (dir-dirs d) fn))]
;    [else false]))
;
;(define (find2-dir d fn)
;  (cond
;    [(find-files? (dir-files d) fn) (cons (dir-name d) empty)]
;    [else (cons (dir-name d) (find2-dirs (dir-dirs d) fn))]))
;
;(define (find2-dirs dirs fn)
;  (cond
;    [(empty? dirs) empty]
;    [(find? (first dirs) fn) (find2-dir (first dirs) fn)]
;    [else (find2-dirs (rest dirs) fn)]))
;
;(define (how-many2 d fn)
;  (+ (how-many2-dirs (dir-dirs d) fn)
;     (how-many2-files (dir-files d) fn)))
;  
;(define (how-many2-files files fn)
;  (cond
;    [(empty? files) 0]
;    [(symbol=? (file-name (first files)) fn) (add1 (how-many2-files (rest files) fn))]
;    [else (how-many2-files (rest files) fn)]))
;
;(define (how-many2-dirs dirs fn)
;  (cond
;    [(empty? dirs) 0]
;    [else (+ (how-many2 (first dirs) fn)
;             (how-many2-dirs (rest dirs) fn))]))
;
;(check-expect (how-many2 model3-TS 'part3) 1)
;(check-expect (how-many2 model3-TS 'read!) 2)
;
;
;(check-expect (find2 model3-TS 'part3) (list (list 'TS 'Text)))
;(check-expect (find2 model3-TS 'read!) (list (list 'TS) (list 'TS 'Libs 'Docs)))
;(check-expect (find2 model3-TS 'hang) (list (list 'TS 'Libs 'Code)))
