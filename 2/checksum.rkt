#lang racket


(define (read-lines filename)
  (string-split (file->string filename) "\n"))



(define (largest-diff lst)
  (- (apply max (map (lambda (x) (string->number x)) lst))	
  (apply min (map (lambda (x) (string->number x)) lst))))


(define (add-if-divisible pair)
  (define x (string->number (first pair)))
  (define y (string->number (last pair)))
  (cond
    [(zero? (remainder  x y))
      (/ x y)]
    [(zero? (remainder y x))
      (/ y x)]
    [else 0]))


(define (get-fraction lst)
  (for/fold ([sum 0])
    ([pair (combinations lst 2)])
    (+ sum (add-if-divisible pair))))

				
(define (checksum filename by-row-function)
  (define input-lines (read-lines filename))
  (for/fold ([sum 0])
    ([line input-lines])
    (+ sum (by-row-function (string-split line)))))

(checksum "input_test.txt" largest-diff) ; = 18
(checksum "input.txt" largest-diff) ; = 42378 


(checksum "input_test2.txt" get-fraction) ; = 9
(checksum "input.txt" get-fraction) ; = 246

