#lang racket



(define (layer-to-dim layer-number)
	(cond
	  [(equal? layer-number 1) 1]
	  [else (- (* layer-number 2) 1)]))

(define (square x)
  (* x x))


; layer / max / min / dim
; 1 	1 		1 		1x1
; 2 	9 		2 		3x3
; 3 	25 		10 		5x5
; 4 	49 		26 		7x7
(define (get-layer-for-number number layer-to-check)
	(define dim-of-layer (layer-to-dim layer-to-check))
	(define layer-min (max 1 (+ 1 (square (- dim-of-layer 2)))))
	(define layer-max (square dim-of-layer))
	(cond
	  [(<= layer-min number layer-max) 
	  	(get-distance-from-center layer-to-check number layer-max layer-min)]
	  ;[(equal? layer-to-check 3) layer-min]
	  [else (get-layer-for-number number (+ layer-to-check 1))]))

(define (min-diff t b l r number)	
	(min 
	(abs (- t number))
	(abs (- b number))
	(abs (- l number))
	(abs (- r number))))
	

(define (get-distance-from-center layer number lmax lmin)
	(define dim (layer-to-dim layer))
	(define mid-bot (- lmax (- layer 1))) ; bottom centroid
	(define mid-top (- mid-bot (* 4 (- layer 1)))) ; top centroid
	(define mid-left (+ lmin (- layer 2))) ; right centroid
	(define mid-right (+ mid-left (* 4 (- layer 1)))) ;left-centroid
;	(define steps-to-nearest-mid (min 
;	(abs (- mid-bot number))
;	(abs (- mid-top number))
;	(abs (- mid-right number))
;	(abs (- mid-left number))))
	(define steps-to-nearest-mid (min-diff mid-top mid-bot mid-right mid-left number))
	(define distance (- (+ layer steps-to-nearest-mid) 1))

	distance)


(define (spiral-mem number)
	(cond 
		[(<= number 1) 0]
		[else (get-layer-for-number number 1)]))


;; INPUT TO SOLVE:  325489
(printf "sprial-mem(~a) = ~a\n" 5 (spiral-mem 5)) ; 2
(printf "sprial-mem(~a) = ~a\n" 12 (spiral-mem 12)) ; 3
(printf "sprial-mem(~a) = ~a\n" 23 (spiral-mem 23)) ; 2

(printf "sprial-mem(~a) = ~a\n" 31 (spiral-mem 31)) ; 6

(printf "sprial-mem(~a) = ~a\n" 1 (spiral-mem 1)) ; 0
(printf "sprial-mem(~a) = ~a\n" 325489 (spiral-mem 325489)) ; 552

