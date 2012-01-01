;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch11) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp") (lib "arrow.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp") (lib "arrow.ss" "teachpack" "htdp")))))
;; ex 11.2.1
;; repeat : number symbol => list
(define (repeat n s)
  (cond
    [(zero? n) empty]
    [else (cons s (repeat (sub1 n) s))]))

;; test
(check-expect (repeat 0 'hello) empty)
(check-expect (repeat 2 'a) (cons 'a (cons 'a empty)))

;; ex 11.2.2

;; f : number -> number
(define (f x)
  (+ (* 3 (* x x)) 
     (+ (* -6 x)
        -1)))

;; tabulate-f : natural-number -> list-of-posn
(define (tabulate-f n)
  (cond
    [(zero? n) empty]
    [else
     (cons (make-posn n (f n))
           (tabulate-f (sub1 n)))]))

;; test
(check-expect (tabulate-f 0) empty)
(check-expect (tabulate-f 2) (cons (make-posn 2 (f 2))
                                   (cons (make-posn 1 (f 1))
                                         empty)))

;; ex 11.2.3

;; from ch10
(define-struct circle (center radius color))
(define-struct rectangle (nw width height color))
(define FACE
  (cons (make-circle (make-posn 50 50) 40 'red)
        (cons (make-rectangle (make-posn 30 20) 5 5 'blue)
              (cons (make-rectangle (make-posn 65 20) 5 5 'blue)
                    (cons (make-rectangle (make-posn 40 75) 20 10 'red)
                          (cons (make-rectangle (make-posn 45 35) 10 30 'blue)
                                empty))))))
(define (draw-shape s)
  (cond
    [(circle? s) (draw-a-circle s)]
    [(rectangle? s) (draw-a-rectangle s)]))
(define (draw-a-circle s)
  (draw-circle (circle-center s)
               (circle-radius s)
               (circle-color s)))
(define (draw-a-rectangle s)
  (draw-solid-rect (rectangle-nw s)
                   (rectangle-width s)
                   (rectangle-height s)
                   (rectangle-color s)))
(define (draw-losh losh)
  (cond
    [(empty? losh) true]
    [else
     (and (draw-shape (first losh))
          (draw-losh (rest losh)))]))
(define (translate-shape delta a-shape)
  (cond
    [(circle? a-shape) (translate-circle delta a-shape)]
    [(rectangle? a-shape) (translate-rectangle delta a-shape)]))
(define (translate-circle d s)
  (make-circle
   (make-posn (+ d  (posn-x (circle-center s)))
              (posn-y (circle-center s)))
   (circle-radius s)
   (circle-color s)))
(define (translate-rectangle d s)
  (make-rectangle
   (make-posn (+ d (posn-x (rectangle-nw s)))
              (posn-y (rectangle-nw s)))
   (rectangle-width s)
   (rectangle-height s)
   (rectangle-color s)))
(define (translate-losh delta losh)
  (cond
    [(empty? losh) empty]
    [else
     (cons (translate-shape delta (first losh))
           (translate-losh delta (rest losh)))]))
(define (clear-shape s)
  (cond
    [(circle? s) (clear-a-circle s)]
    [(rectangle? s) (clear-a-rectangle s)]))
(define (clear-a-circle s)
  (clear-circle (circle-center s)
                (circle-radius s)
                (circle-color s)))
(define (clear-a-rectangle s)
  (clear-solid-rect (rectangle-nw s)
                    (rectangle-width s)
                    (rectangle-height s)
                    (rectangle-color s)))
(define (clear-losh losh)
  (cond
    [(empty? losh) true]
    [else
     (and (clear-shape (first losh))
          (clear-losh (rest losh)))]))
(define (draw-and-clear-picture picture)
  (and
   (draw-losh picture)
   (sleep-for-a-while 0.05)
   (clear-losh picture)))
(define (move-picture delta picture)
  (cond
    [(draw-and-clear-picture picture) (translate-losh delta picture)]
    [else picture]))

;; apply-n : natural-number -> ???
(define (apply-n n)
  (cond
    [(zero? n) FACE]
    [else
     (move-picture 1 (apply-n (sub1 n)))]))

;; test
;(start 500 100)
;(draw-losh (apply-n 3)) ;; =>
;(draw-losh (move-picture 1
;                         (move-picture 1
;                                       (move-picture 1 FACE))))

;; ex 11.2.4
;; depth : deep-list -> number
(define (depth dl)
  (cond
    [(symbol? dl) 0]
    [else
     (add1 (depth (first dl)))]))

;; test
(check-expect (depth 's) 0)
(check-expect (depth (cons 's empty)) 1)
(check-expect (depth (cons (cons 's empty) empty)) 2)
(check-expect (depth (cons (cons (cons (cons 's empty) empty) empty) empty)) 4)

;; make-deep : symbol natural-number -> deep-list
(define (make-deep s n)
  (cond
    [(zero? n) s]
    [else
     (cons (make-deep s (sub1 n)) empty)]))

;; example
(check-expect (make-deep 'y 3)
              (cons (cons (cons 'y empty) empty) empty))

;; ex 11.3.1
(define (random-n-m n m)
  (+ (random (- m n)) n))
;; n 이상 m 미만에 있는 임의의 자연수를 출력한다.

;; ex 11.3.2
;; tie-dyed : number -> number
(define (tie-dyed n)
  (cond
    [(zero? n) empty]
    [else
     (cons (random-n-m 20 120)
           (tie-dyed (sub1 n)))]))
;; ex 9.5.8에 적용하는 코드는 ch9.rkt 참조

;; ex 11.3.3
;; create-temps : number number number -> list-of-numbers
(define (create-temps n low high)
  (cond
    [(zero? n) empty]
    [else
     (cons (random-n-m low high)
           (create-temps (sub1 n) low high))]))

;- Can we simply feed the result of create-temps into check-range or do we need to know the list that create-temps produced?
;할 수 있다.
;
;- Are there values for low and high such that we don't need to know the result of create-temps and yet we can predict the result of the test?
;(check-range (create-temps 20 l1 h1) l2 h2)가 실행될 때 l1 < h1, l2 < h2, l1 >= l2, h1 <= h2 이면 항상 참이 나온다. 특정 상황에서는 예측 가능해 진다.
;
;- Which function tests which?
;check-range 함수가 리스트 내에 있는 모든 숫자들이 특정 범위안에 존재하는지 테스트한다.
;
;- What does this tell us about testing with automatically generated test data?
;테스트 데이터는 테스트 대상 함수에서 다양한 결과가 나오도록 준비되어야 한다.
;잘못 설계된 테스트 데이터는 함수의 잘못된 동작을 확인하지 못할 수 있다.

;; ex 11.3.4
;; create-prices : number -> list-of-numbers
;; 문제 해석 : 원소 개수가 n개인 리스트를 반환한다.
;;            리스트의 값은 0.1부터 10 범위의 임의의 수이며 0.1단위로 증가한다.
(define (create-prices n)
  (cond
    [(zero? n) empty]
    [else
     (cons (/ (+ (random 100) 1) 10)
           (create-prices (sub1 n)))]))

;; examples
; (create-prices 3)

(define (dollar-store? prices)
  (cond
    [(empty? prices) true]
    [else
     (and (<= (first prices) 1)
          (dollar-store? (rest prices)))]))

(dollar-store? (create-prices 20))

;; ex 11.3.5
(define canvas-width 300)
(define canvas-height 300)
(define row-size 25)
(define col-size 25)
;; draw-board : number -> true
(define (draw-board n)
  (cond
    [(zero? n) true]
    [else
     (and
      (and
       (draw-solid-line (make-posn (* n row-size) 0)
                        (make-posn (* n row-size) canvas-height))
       (draw-solid-line (make-posn 0 (* n col-size))
                        (make-posn canvas-width (* n col-size))))
      (draw-board (sub1 n)))]))
  
;; draw-balloons : number-of-ballons -> true
(define (draw-balloons n)
  (cond
   [(zero? n) true]
   [else
    (and
     (draw-solid-disk (make-posn (random canvas-width)
                                 (random canvas-height)) 3 'red)
     (draw-balloons (sub1 n)))]))

;(start canvas-width canvas-height)
;(draw-board 10)
;(draw-balloons 10)

;; ch 11.4

;; ! : number -> number
(define (! n)
  (cond
    [(zero? n) 1]
    [else (* n (! (sub1 n)))]))

;; ex 11.4.1
;(! 2)    ;; => 2
;(! 10)   ;; => 3628800
;(! 100)  ;; => 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
;(! 1000) ;; 402387260077093773543702433923003985719374864210714632543799910429938512398629020592044208486969404800479988610197196058631666872994808558901323829669944590997424504087073759918823627727188732519779505950995276120874975462497043601418278094646496291056393887437886487337119181045825783647849977012476632889835955735432513185323958463075557409114262417474349347553428646576611667797396668820291207379143853719588249808126867838374559731746136085379534524221586593201928090878297308431392844403281231558611036976801357304216168747609675871348312025478589320767169132448426236131412508780208000261683151027341827977704784635868170164365024153691398281264810213092761244896359928705114964975419909342221566832572080821333186116811553615836546984046708975602900950537616475847728421889679646244945160765353408198901385442487984959953319101723355556602139450399736280750137837615307127761926849034352625200015888535147331611702103968175921510907788019393178114194545257223865541461062892187960223838971476088506276862967146674697562911234082439208160153780889893964518263243671616762179168909779911903754031274622289988005195444414282012187361745992642956581746628302955570299024324153181617210465832036786906117260158783520751516284225540265170483304226143974286933061690897968482590125458327168226458066526769958652682272807075781391858178889652208164348344825993266043367660176999612831860788386150279465955131156552036093988180612138558600301435694527224206344631797460594682573103790084024432438465657245014402821885252470935190620929023136493273497565513958720559654228749774011413346962715422845862377387538230483865688976461927383814900140767310446640259899490222221765904339901886018566526485061799702356193897017860040811889729918311021171229845901641921068884387121855646124960798722908519296819372388642614839657382291123125024186649353143970137428531926649875337218940694281434118520158014123344828015051399694290153483077644569099073152433278288269864602789864321139083506217095002597389863554277196742822248757586765752344220207573630569498825087968928162753848863396909959826280956121450994871701244516461260379029309120889086942028510640182154399457156805941872748998094254742173582401063677404595741785160829230135358081840096996372524230560855903700624271243416909004153690105933983835777939410970027753472000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

;; ex 11.4.2
;; product : number(n) number(m) -> number
;; (produce n m), n < m => (n+1) * (n+2) * (n+3) * ... (m-1) * m
(define (product n m)
  (/ (! m) (! n)))

(product 3 5) ;; => (/ (* 1 2 3 4 5) (* 1 2 3)) = (* 4 5) = 20

;; ex 11.4.3
;; product-from-minus-11 : number[>= -11] -> number
(define (product-from-minus-11 n)
  (cond
    [(= n -11) 1]
    [else
     (* n (product-from-minus-11 (sub1 n)))]))
;; test
(product-from-minus-11 -11) ;; 1
(product-from-minus-11 -10) ;; -10
(product-from-minus-11 -1)  ;; -1 * -2 * -3 * ... * -10 = 3628800
(product-from-minus-11 100) ;; 0, 0이상이면 무조건 0이다.

;; ex 11.4.4
;; tabulate-f20 : n -> list-of-posn
(define (tabulate-f20 n)
  (cond
    [(<= n 20) empty]
    [else
     (cons (make-posn n (f n))
           (tabulate-f20 (sub1 n)))]))

;; tests
(tabulate-f20 20) ;; empty
(tabulate-f20 22) ;; =>
(cons (make-posn 22 (f 22))
      (cons (make-posn 21 (f 21)) empty))

;; ex 11.4.5
;; tabulate-f-lim number[limit] number[>=limit] => list-of-posn
(define (tabulate-f-lim n limit)
  (cond
    [(<= limit n) empty]
    [else
     (cons (make-posn limit (f limit))
           (tabulate-f-lim n (sub1 limit)))]))

;; tests
(tabulate-f-lim 20 20) ;; empty
(tabulate-f-lim 20 22) ;; =>
(cons (make-posn 22 (f 22))
      (cons (make-posn 21 (f 21)) empty))
(tabulate-f-lim 0 3)

;; ex 11.4.6
;; tabulate-f-up-to-20 : Number[<= 20] => number
(define (tabulate-f-up-to-20 n)
  (cond
    [(= n 20) empty]
    [else
     (cons (make-posn n (f n))
           (tabulate-f-up-to-20 (add1 n)))]))
;; tests
(tabulate-f-up-to-20 18)
(cons (make-posn 18 (f 18))
      (cons (make-posn 19 (f 19)) empty))

;; ex 11.4.7
;; is-not-divisible-by<=i : i[i >= 1] m[i < m] => boolean
(define (is-not-divisible-by<=i i m)
  (cond
   [(= i 1) true]
   [else
    (cond
      [(= (remainder m i) 0) false]
      [else (is-not-divisible-by<=i (sub1 i) m)])]))

(is-not-divisible-by<=i 2 3)  ;; true
(is-not-divisible-by<=i 7 8)  ;; false
(is-not-divisible-by<=i 9 11) ;; true
;; true 이면 prime number 이다.

;; prime? : number -> boolean
(define (prime? n)
  (is-not-divisible-by<=i (ceiling (/ n 2)) n))
;; test
(prime? 11) ;; true
(prime? 28) ;; false

;; ex 11.5.1
;; add : Number Number -> Number
;; x에 add1을 n번 수행한다. !!!
(define (add n x)
  (cond
    [(zero? n) x]
    [else
     (add1 (add (sub1 n) x))]))

;; test
(add 10 2) ;; 12

;; ex 11.5.2
;; multiply-by-pi : Number -> Number
(define (multiply-by-pi n)
  (cond
    [(zero? n) 0]
    [else
     (+ 3.14 (multiply-by-pi (sub1 n)))]))

;; tests
(= (multiply-by-pi 0) 0)
(= (multiply-by-pi 2) 6.28)
(= (multiply-by-pi 3) 9.42)
(= (multiply-by-pi 10) 31.4)

;; multiply : Number Number -> Number
(define (multiply n x)
  (cond
    [(= n 1) x]
    [else
     (add x (multiply (sub1 n) x))]))

(= (multiply 2 3) 6)
(= (multiply 5 7) 35)

;; ex 11.5.3
;; exponent : Number Number -> Number
(define (exponent n x)
  (cond
    [(= n 1) x]
    [else
     (multiply x (exponent (sub1 n) x))]))
(= (exponent 2 3) 9)   ;; => 3^2
(= (exponent 5 3) 243) ;; => 3^5

;; ex 11.5.4
;; 이거 맞는거야???
;; 0 => empty
;; 3 => (cons (cons (cons 1 empty) empty) empty)
;; 8 => (cons (cons (cons (cons (cons (cons (cons (cons 1 empty) empty) empty)
;;                               empty) empty) empty) empty) empty)
;; addDL : Number Number => Deep-List
;(define (addDL n m)
;  (cond
;    [(zero? n) 1]
;    [else
;     (cons (addDL (sub1 n) m) empty)]))

(define (addDL n m)
  (cond
    [(and (zero? n) (zero? m)) 1]
    [(zero? n) (cons (addDL n (sub1 m)) empty)]
    [(zero? m) (cons (addDL (sub1 n) m) empty)]
    [else (cons (cons (addDL (sub1 n) (sub1 m)) empty) empty)]))

(addDL 1 1) ;; (cons 1 empty) + (cons 1 empty) => (cons (cons 1 empty) empty)
(addDL 2 3) ;; (cons (cons 1 empty) empty) + (cons (cons (cons 1 empty) empty) empty)
            ;; (cons (cons (cons (cons (cons 1 empty) empty) empty) empty) empty)
;; 뭔가 이상하다!!!
