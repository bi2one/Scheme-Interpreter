(define (adjoin-position row col queens) (append (list row) queens))
(define (filteri pred seq) (define (iter index items) (cond ((null? items) (list)) ((pred index (car items)) (cons (car items) (iter (+ index 1) (cdr items)))) (else (iter (+ index 1) (cdr items))))) (iter 1 seq))
(define (safe? k positions) (define queen (car positions)) (define (check-mate col row) (or (= queen row) (= col (abs (- row queen))))) (null? (filteri check-mate (cdr positions))))
(define (queens board-size) (define (queen-cols k) (if (= k 0) (list '()) (filter (lambda (positions) (safe? k positions)) (flatmap (lambda (rest-of-queens) (map (lambda (new-row) (adjoin-position new-row k rest-of-queens)) (enumerate-interval 1 board-size))) (queen-cols (- k 1)))))) (queen-cols board-size))
