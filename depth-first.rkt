#lang racket/base

(require racket/list racket/vector)
(require "candidate.rkt" "testing.rkt")
(module+ test (require rackunit))
(provide solve easy medium-1 medium-2 hard)

;@title[#:tag "depth first"]{Depth First}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/sudoku/blob/main/depth-first.rkt" "depth-first.rkt"]}

;When the board has cells with multiple candidate values, we can solve the puzzle with a depth-first search.
;This means trying a candidate value for each cell, until we find the values that lead to a solution.

;At each level of the search, we look for the next @bold{cell with multiple} values.

(test-case:
 "find multiple"
 (check-equal? (cell-with-multiple #(3 5 1)) 0)
 (check-equal? (cell-with-multiple #(3 2 5 1) 1) 2)
 (check-equal? (cell-with-multiple #(2 5)) 1)
 (check-false (cell-with-multiple #(1 2 4))))

;If the cell value isn't one of the @elemref["single-values"]{single values}, then it must have multiple values.

(define (cell-with-multiple board [start 0])
  (for/first ([i (in-range start (vector-length board))]
              #:unless (vector-member (vector-ref board i) single-values))
    i))

;We'll need a test to see if a cell @bold{has} a @bold{value} as one of its candidates.

(test-case:
 "has value"
 (check-true (has-value? 2 3))
 (check-false (has-value? 2 5)))

(define (has-value? value bits)
  (> (bitwise-and value bits) 0))

;Then we can get a list of the values that are @bold{candidates} for a cell.

(test-case:
 "list of candidates"
 (check-equal? (candidates 3) '(1 2)))

;This uses the list of @elemref["single-values"]{single values} to find out which ones the cell has.

(define (candidates bits)
  (filter-map (λ (value) (and (has-value? value bits) value)) 
              (vector->list single-values)))

;The simplest puzzles to @bold{solve} are ones where no cells have multiple candidates.
;In these case, the puzzle is already solved.

(define easy
  (assign-cell-values
   "43 618 5 2   3 84 7 1 4 6  894     2  3  65    28 1  4928    3  4  83 15   967  8"))

(test-case:
 "solved board is returned"
 (check-equal? (solve easy) easy))

;In some cases, choosing the right value for the first cell with multiple values is enough the solve the puzzle.
;Here we have a couple of examples.
;In the first example, the correct value is the first candidate value.
;In the second example, the correct value is the second candidate value.

(define medium-1
  (assign-cell-values
   " 7926  45342875  6  6 1 7   957  3 1 6  94      1   8      82  2  6    8      634"))

(test-case:
 "first candidate solves"
 (check-equal? (board->string (solve medium-1))
               "179263845\n342875196\n586419723\n495782361\n861394572\n723156489\n654938217\n237641958\n918527634\n"))

(define medium-2
  (assign-cell-values
   " 49   1 6  1 8     3   4 272 54783 13 41 69756 7 9     7    2    32 1      83  19"))

(test-case:
 "second candidate solves"
 (check-equal? (board->string (solve medium-2))
               "849752136\n721683594\n536914827\n295478361\n384126975\n617395482\n178569243\n963241758\n452837619\n"))

;With the hardest puzzles, we need to choose values for multiple cells. 

(define hard
  (assign-cell-values
   "     68  8 6 3  5       2    49          47 2  538     2        19  738 5     1  "))

(test-case:
 "nested solve required"
 (check-equal? (board->string (solve hard))
               "152746893\n876239451\n493158276\n234971568\n981564732\n765382914\n327815649\n619427385\n548693127\n"))

;If there are no cells with multiple values, this means we have reached a solution.
;Otherwise, we have to try the candidate values.

(define (solve board [start 0])
  (let ([position (cell-with-multiple board start)])
    (if position
        (try-candidates board position)
        board)))

;We try each candidate until one of them returns a solution.

(define (try-candidates board position)
    (for/or ([option (candidates (vector-ref board position))])
      (try-candidate option position board)))

;To try a candidate value, we make a copy of the board and set our value on the copy.
;If this results in a valid board, we then try to solve the copy.
;This is where we move to the next level in our depth-first approach.

(define (try-candidate option position board)
  (let ([copy (vector-copy board)])
    (and
     (set-value copy position option)
     (solve copy (add1 position)))))

;So how long does it take to solve our examples?
;The times are in milliseconds.

;@(require scribble/example)
;@(define eg-eval (make-base-eval))
;@examples[#:hidden #:eval eg-eval (require "../sudoku/depth-first.rkt")]
;@examples[
;#:eval eg-eval #:label ""
;(define (time-multiple board count)
;  (time (for ([i count]) (solve board)) "done"))
;(time-multiple easy 1000)
;(time-multiple medium-1 1000)
;(time-multiple medium-2 1000)
;(time-multiple hard 1000)
;]
