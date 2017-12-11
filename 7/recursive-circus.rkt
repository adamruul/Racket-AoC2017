#lang racket

(struct program (name weight children))


(define (read-lines filename)
  (string-split (file->string filename) "\n"))


(define (parse-line line)
  (define list-of-fields (string-split line))
  (define name (first list-of-fields))
  (define weight (string->number (string-trim (string-trim (second list-of-fields) "(" ) ")")))
  (cond
    [(> (length list-of-fields) 2)
      (program name weight (map (lambda (x) (string-trim x ",")) (drop list-of-fields 3)))] 
    [else 
      (program name weight (list))]))


(define (program-member prgrm lst)
  (memf
    (lambda (p) (equal? prgrm (program-name p)))
	lst))


(define (get-root programs-left added-programs last-added)
  (define programs-to-add 
    (filter 
	  (lambda (x) (andmap (lambda (child-name) (program-member child-name added-programs)) (program-children x)))
	  programs-left))
  (cond
    [(empty? programs-to-add) (program-name (first last-added))]
	[else
	  (get-root
	    (remove* programs-to-add programs-left)
	    (append  programs-to-add added-programs)
		programs-to-add)]))


(define (recursive-circus input-file)
  (define programs (map (lambda (line) (parse-line line)) (read-lines input-file))) ; construct list of programs
  (get-root programs (list) (list)))


(define root-program (recursive-circus "input.txt")) ; eqgvf
(printf "~a\n" root-program)





; part 2 -------------------------------------------------------------------------------
(define (get-node node-name tree)
(findf (lambda (x) (equal? (program-name x) node-name)) tree))

(define (construct-tree input-file root-name)
  (define programs (map (lambda (line) (parse-line line)) (read-lines input-file))) ; construct list of programs
  (define root-node (findf (lambda (x) (equal? (program-name x) root-name)) programs))
  (foo  
    programs
    root-node
    (list)))

(define (duplicates xs)
  (set->list (list->set 
     (for/fold ([xs xs]) ([u (set->list (list->set xs))])
	      (remove u xs)))))
(define (uniques xs)
  (remove* (duplicates xs) xs))


(define (foo tree root last-weight-series)
  (define child-node-names
  (program-children root))
  (define weights (map (lambda (c) (summed-weight c tree)) (program-children root)))
  
  (cond 
    [(empty? (uniques weights)) 
	  (- (program-weight root) (- (first (uniques last-weight-series)) (check-duplicates last-weight-series)))]
  [else
  (begin
   (define diffing-node (findf
     (lambda (y) (equal? (second y) (first (uniques weights))))
	 (map list (program-children root) weights)))
   (foo tree (get-node (first diffing-node) tree) weights))]))


(define (summed-weight node-name tree)
(define node (findf (lambda (x) (equal? (program-name x) node-name)) tree))
  (cond 
    [(empty? (program-children node)) (program-weight node)]
  [else 
    (foldl + (program-weight node) (map (lambda (child) (summed-weight child tree)) (program-children node)))]))


(construct-tree "input.txt" root-program)
