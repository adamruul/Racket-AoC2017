#lang racket


(define (read-list filename)
  (map (lambda (x) (string->number x))
  (string-split (file->string filename))))

(define (get-next-index current-index num-banks)
	(cond
      [(equal? current-index (- num-banks 1)) 0]
	  [else (+ current-index 1)]))

(define (re-allocate list-of-banks num-blocks index num-banks)
  (define next-index (get-next-index index num-banks))
  (cond
    [(equal? num-blocks 0) list-of-banks]
  [else
    (re-allocate
	  (list-set list-of-banks index (+ (list-ref list-of-banks index) 1))
	  (- num-blocks 1)
	  next-index
	  num-banks)]))


(define (memory-allocation list-of-banks num-allocations discovered-states first-found)
  (define start-blocks (apply max list-of-banks))
  (define start-index (index-of list-of-banks start-blocks)) 
  (cond
    [(equal? (member list-of-banks discovered-states) #f)
	  (memory-allocation
	    (re-allocate
		  (list-set list-of-banks start-index 0)
		  start-blocks
		  (get-next-index start-index (length list-of-banks))
		  (length list-of-banks))
		(+ num-allocations 1)
		(cons list-of-banks discovered-states)
		first-found)]
	[else 
	  (cond
	    [(false? first-found)
		(begin
		  (printf "~a\n" num-allocations)
		  (memory-allocation list-of-banks 0 (list) #t))]
	    [else num-allocations])]))

(define (memory-debugger filename)
  (define input-list (read-list filename))
  (memory-allocation input-list 0 (list) #f))


(memory-debugger "input.txt") ; 14029, 2765

