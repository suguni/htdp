;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ch24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; ex 24.0.9
((lambda (x y) (+ x (* x y))) 1 2)

((lambda (x y) (+ x (local ((define x (* y y)))
                        (+ (* 3 x) (/ 1 x))))) 1 2)
((lambda (x y)
     (+ x
        ((lambda (x)
           (+ (* 3 x)
              (/ 1 x)))
         (* y y))))
   1 2)