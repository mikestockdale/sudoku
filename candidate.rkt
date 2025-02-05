#lang racket/base

(provide single-values assign-cell-values set-value board->string easy-1 medium-1)
(require racket/format racket/list racket/vector srfi/13)
(require "testing.rkt")
(module+ test (require rackunit))

;@title[#:tag "candidates"]{Candidates}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/sudoku/blob/main/candidate.rkt" "candidate.rkt"]}

;I have an idea of an integer for each cell on the board, using a bit for each candidate value from one to nine.
;On an empty board, every cell would have all nine bits on, since all values are valid candidates for every cell.
;If we use a vector of 81 cells for the board, an @bold{empty board} would look like this:

(define (empty-board) (make-vector 81 511))

;When a cell has been assigned a value, it will have only a single bit set.
;This will be one of these @elemtag["single-values"]{@bold{single values}}.

(define single-values #(1 2 4 8 16 32 64 128 256))

;To update the board as cell values are assigned, we'll need a couple of functions.

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

;A useful function for testing converts a @bold{board} to a @bold{string}.
;A cell that has been assigned a value appears as the digit 1-9.
;A cell that has multiple candidate values appears as the hex value of the bits.
;This requires 3 characters to show the 9 bits.
;The first character is always 0 or 1, and to provide a visual separation, this character appears as a superscript.
;Each row is suffixed with a new line character.

(test-case:
 "board to string"
 (check-equal? (board->string #(1 256 3 511 1 1 1 1 1 2 2 2 2 2 2 2 2 2))
               "19°03¹ff11111\n222222222\n"))

(define (board->string board)
  (define (hex prefix value)
    (string-append prefix (string-pad (number->string value 16) 2 #\0)))
  (define (cell->string cell-value)
    (let ([single (vector-member cell-value single-values)])
      (if single
          (number->string (+ single 1))
          (if (< cell-value 256)
              (hex "°" cell-value)
              (hex "¹" (- cell-value 256))))))
  (string-join
   (for/list ([i (in-range 0 (vector-length board) 9)])
     (string-join
      (for/list ([cell (in-vector board i (+ i 9))]) (cell->string cell)) ""))
   "\n" 'suffix))

;To @bold{assign} a @bold{cell value}, we set the single bit and clear bits in all the related cells.

(test-case:
 "assign value"
 (check-equal? (board->string (assign-cell-value (empty-board) 0 8))
               (string-append
                "8¹7f¹7f¹7f¹7f¹7f¹7f¹7f¹7f\n"
                (xsubstring "¹7f¹7f¹7f¹ff¹ff¹ff¹ff¹ff¹ff\n" 0 56)
                (xsubstring "¹7f¹ff¹ff¹ff¹ff¹ff¹ff¹ff¹ff\n" 0 168))))

(define (assign-cell-value board position value)
  (set-value board position (set-single-bit value)))

(define (set-value board position bit-value)
  (define (apply-mask target-cell mask)
    (let* ([original-value (vector-ref board target-cell)]
           [new-value (clear-bit mask original-value)])
      (and (> new-value 0)
           (unless (= new-value original-value)
             (vector-set! board target-cell new-value)
             (when (vector-member new-value single-values)
               (set-value board target-cell new-value))))))   
  (vector-set! board position bit-value)
  (and
   (for/and ([related-cell (in-list (vector-ref related-cells position))])
     (apply-mask related-cell bit-value))
   board))

;As we @bold{assign cell values}, the number of candidates for a related cell may be reduced to just one.
;This means we have derived the value for that cell, and can update its related cells recursively.

;In this example, eight of the nine values in the top left square are assigned, so the ninth value can be derived.
;Seven of the nine values in the top row are assigned, and the derived value is the eighth.
;So the ninth value in the top row can also be derived.

(test-case:
 "assign derived"
 (check-equal? (board->string (assign-cell-values "12 45678 456      789"))
               (string-append
                "123456789\n456¹c7¹c7¹c7°07°07°07\n789°07°07°07°3f°3f°3f\n"
                (xsubstring "¹b6¹6d°db¹f7¹ef¹df¹bf¹7f°ff\n" 0 168))))

;If a conflicting assignment is made, the result is false.

(test-case:
 "conflict"
 (check-false (assign-cell-values "11")))

(define (assign-cell-values cells)
  (let ([board (empty-board)])
    (and
     (for/and ([i (string-length cells)] [value cells] #:when (char-numeric? value))
       (assign-cell-value board i (- (char->integer value) (char->integer #\0))))
     board)))

;This is enough to solve some easy puzzles.

(define easy-1
  (assign-cell-values
   "43 618 5 2   3 84 7 1 4 6  894     2  3  65    28 1  4928    3  4  83 15   967  8"))

;@(require scribble/example)
;@(define eg-eval (make-base-eval))
;@examples[#:hidden #:eval eg-eval (require "../sudoku/candidate.rkt")]
;@examples[
;#:eval eg-eval #:label ""
;(display (board->string easy-1))
;]

;Medium puzzles will require more work to solve.

(define medium-1
  (assign-cell-values
   " 49   1 6  1 8     3   4 272 54783 13 41 69756 7 9     7    2    32 1      83  19"))

;@examples[
;#:eval eg-eval #:label ""
;(display (board->string medium-1))
;]
