#lang racket


(define (read-lines filename)
  (string-split (file->string filename) "\n"))


(define (check-passphrases filename criteria-fun)  
  (define input-lines (read-lines filename))
  (for/fold ([passing-passphrases 0])
    ([line input-lines])
   	(cond 
	  [(criteria-fun (string-split line)) (+ passing-passphrases 0)]
	  [else (+ passing-passphrases 1)])))


(define (has-duplicate-words words)
	(cond
  	  [(equal? (check-duplicates words) #f) #f]
      [else #t]))


(define (is-anagram pair-of-words)
		(define l1 (string-split (first pair-of-words) ""))
		(define l2 (string-split (last pair-of-words) ""))
		(equal? (sort l1 string<?) (sort l2 string<?)))


(define (has-anagram-words words)
	(for/first
	([pair (combinations words 2)] #:when (is-anagram pair))
	  #t))		



(check-passphrases "input.txt" has-duplicate-words) ; = 451
(check-passphrases "input.txt" has-anagram-words) ; = 223

