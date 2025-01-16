#lang racket/base
(require racket/format racket/list racket/vector)
(require "testing.rkt")
(module+ test (require rackunit))

;@title{Sudoku}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/sudoku/blob/main/main.rkt" "main.rkt"]}
;Well, everyone has to do Sudoku eventually, don't they?

;I have an idea of an integer for each cell on the board, using a bit for each candidate value from one to nine.
;On an empty board, every cell would have all nine bits on, since all values are valid candidates for every cell.
;If we use a vector of 81 cells for the board, an @bold{empty board} would look like this:

(define (empty-board) (make-vector 81 511))

;When a cell has been set to a value, it will have only a single bit set.
;This is will be one of these @bold{single values}.

(define single-values #(1 2 4 8 16 32 64 128 256))

;To update the board as cell values are set, we'll need a couple of functions.

;First, for the cell that is assigned a value, we want to @bold{set} a @bold{single bit} indicating there is only one valid value for the cell.
;@margin-note{@racket[test-case:] is a macro that makes it easier to interleave test and production code: source at @hyperlink["https://github.com/mikestockdale/sudoku/blob/main/testing.rkt" "testing.rkt"]}

(test-case:
 "set single bit"
 (check-equal? (set-single-bit 1) 1)
 (check-equal? (set-single-bit 9) 256))

(define (set-single-bit value) (arithmetic-shift 1 (- value 1)))

;Second, for cells in the same row, column, or square, we want to @bold{clear} the @bold{bit}, removing one candidate value for the cell.

(test-case:
 "clear bit"
 (check-equal? (clear-bit 256 511) 255)
 (check-equal? (clear-bit 1 15) 14)
 (check-equal? (clear-bit 1 2) 2))

(define (clear-bit bit current-bits)
  (bitwise-and current-bits (bitwise-not bit)))

;Now we need to determine which cells are in the same row, column, or square as a given cell.

;The @bold{cells in} a @bold{row} are numbered consecutively.

(test-case:
 "cells in row"
 (check-equal? (cells-in-row 0) '(0 1 2 3 4 5 6 7 8))
 (check-equal? (cells-in-row 8) '(0 1 2 3 4 5 6 7 8))
 (check-equal? (cells-in-row 9) '(9 10 11 12 13 14 15 16 17))
 (check-equal? (cells-in-row 80) '(72 73 74 75 76 77 78 79 80)))

(define (cells-in-row cell-number)
  (for/list ([i 9]) (+ i (* 9 (quotient cell-number 9)))))

;The @bold{cells in} a @bold{column} are numbered in increments of 9.

(test-case:
 "cells in column"
 (check-equal? (cells-in-column 0) '(0 9 18 27 36 45 54 63 72))
 (check-equal? (cells-in-column 72) '(0 9 18 27 36 45 54 63 72))
 (check-equal? (cells-in-column 1) '(1 10 19 28 37 46 55 64 73))
 (check-equal? (cells-in-column 80) '(8 17 26 35 44 53 62 71 80)))

(define (cells-in-column cell-number)
  (for/list ([i 9]) (+ (* i 9) (remainder cell-number 9))))

;The @bold{cells in} a @bold{square} are ... a bit trickier.

(test-case:
 "cells in square"
 (check-equal? (cells-in-square 0) '(0 1 2 9 10 11 18 19 20))
 (check-equal? (cells-in-square 20) '(0 1 2 9 10 11 18 19 20))
 (check-equal? (cells-in-square 3) '(3 4 5 12 13 14 21 22 23))
 (check-equal? (cells-in-square 27) '(27 28 29 36 37 38 45 46 47))
 (check-equal? (cells-in-square 80) '(60 61 62 69 70 71 78 79 80)))

(define (cells-in-square cell-number)
  (for*/list ([i 3] [j 3])
    (+ j
       (* 9 i)
       (* 3 (quotient (remainder cell-number 9) 3))
       (* 27 (quotient cell-number 27)))))

;Now we can create a list of @bold{related cells} for each position on the board - the cells in the same row, column, or square.

(define related-cells
  (for/vector ([i 81])
    (remove
     i (remove-duplicates
        (append (cells-in-row i) (cells-in-column i) (cells-in-square i))))))


(define (set-value board position bit-value)
  (define (apply-mask related mask)
    (for ([target-cell (in-list related)])
      (let* ([original-value (vector-ref board target-cell)]
             [new-value (clear-bit mask original-value)])
        (when (= new-value 0)
          (displayln (~a "error " new-value " " original-value " " target-cell " " mask)))
        (unless (= new-value original-value)
          (vector-set! board target-cell new-value)
          (when (vector-member new-value single-values)
            (displayln (~a "set " target-cell " to " (add1 (vector-member new-value single-values))))
            (set-value board target-cell new-value))))))   
  (let ([mask (bitwise-not bit-value)])
    (vector-set! board position bit-value)
    (apply-mask (vector-ref related-cells position) bit-value)))

(define (set-square board position value)
  (displayln (~a "given " position " is " value))
  (set-value board position (set-single-bit value)))

(define (set-squares board squares)
  (for ([square squares])
    (set-square board (first square) (second square))))

(define (medium-1)
  (set-squares
   (empty-board)
   '((1 4) (2 9) (6 1) (8 6) (11 1) (13 8) (19 3) (23 4) (25 2) (26 7) (27 2) (29 5) (30 4)
           (31 7) (32 8) (33 3) (35 1) (36 3) (38 4) (39 1) (41 6) (42 9) (43 7) (44 5)
           (45 6) (47 7) (49 9) (55 7) (60 2) (65 3) (66 2) (68 1) (75 8) (76 3) (79 1) (80 9))))
