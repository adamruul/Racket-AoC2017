#lang racket

(define (char-to-int c)
  (- (char->integer c) 48))


;; Part 1 -----------------------------------------------------------

(define (add-if-eq x y)
  (cond 
    [(eq? x y)
      (char-to-int x)]
    [else 0]))


(define (pair-sum lst)
  (for/fold ([sum 0])
    ([prev lst]
    [current (cdr lst)])
    (+ sum (add-if-eq prev current))))


(define (captcha input-file)
  (define input-list (string->list (file->string input-file)))
  (+ (add-if-eq (first input-list) (last input-list))
  (pair-sum input-list)))


;; Part 2 -----------------------------------------------------------

;; Since the sequence if 'circular', we can just add each digit twice.
(define (sum-if-eq x y)
  (cond
    [(eq? x y)
      (+ (char-to-int x) (char-to-int y))]
    [else 0]))


(define (split-sum lst)
  (for/fold ([sum 0])
    ([e1 (take lst (/ (length lst) 2))]
    [e2 (drop lst (/ (length lst) 2))])
    (+ sum (sum-if-eq e1 e2))))


(define (captcha-2 input-file)
  (split-sum (string->list (file->string input-file))))



(captcha "input_1.txt") ; = 1390
(captcha-2 "input_2.txt") ; = 1232

