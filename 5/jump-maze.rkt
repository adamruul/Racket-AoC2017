#lang racket


(define (read-list filename)
  (map (lambda (x) (string->number x))
  (string-split (file->string filename) "\n")))


(define (jump-maze maze current-pos steps-to-jump max-pos min-pos jumps-made maze-updater)
(cond
  [(> (+ current-pos steps-to-jump) max-pos) (+ jumps-made 1)]
  [(< (+ current-pos steps-to-jump) min-pos) (+ jumps-made 1)]
  [else
    (jump-maze
	 (maze-updater maze current-pos)
	 (+ current-pos steps-to-jump)
	 (list-ref (maze-updater maze current-pos) (+ current-pos steps-to-jump))
	 max-pos min-pos
	 (+ jumps-made 1)
	 maze-updater)]))

(define (update-maze maze pos val)
  (list-set maze pos (+ (list-ref maze pos) val)))


(define (update-maze-1 maze pos)
	(update-maze maze pos 1))

(define (update-maze-2 maze pos)
(cond
  [(>= (list-ref maze pos) 3) (update-maze maze pos -1)]
  [(<= (list-ref maze pos) -3) (update-maze maze pos 1)]
  [else (update-maze maze pos 1)]))


(define (solve-maze input-file maze-updater)
  (define input-list (read-list input-file))
  (jump-maze input-list 0 (first input-list) (- (length input-list) 1) 0 0 maze-updater))

(solve-maze "input.txt" update-maze-1) ; = 343467
(solve-maze "input.txt" update-maze-2) ; = 24774780 (WARNING TAKES LONG TIME)

