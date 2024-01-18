;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)


; =========================
; constants

(define ROWS
  (build-list 9 (lambda (y) (build-list 9 (lambda (x) (+ (* 9 y) x))))))
(define COLUMNS
  (build-list 9 (lambda (y) (build-list 9 (lambda (x) (+ y (* 9 x)))))))
(define BLOCKS
  (foldr
   append '()
   (build-list
    3 (lambda (v)
        (build-list
         3 (lambda (w)
             (foldr
              append '()
              (build-list
               3 (lambda (y)
                   (build-list
                    3 (lambda (x)
                        (+ (* 9 (+ y (* 3 v))) x (* 3 w )))))))))))))
(define ZONES (append ROWS COLUMNS BLOCKS))
(define ITERATOR (build-list 81 identity))
(define INTS (build-list 9 add1))



; =========================
; functions


(define (solve bd)
  ; [ListOf N] -> [ListOf N]
  ;  solves the sudoku puzzle by determining the next square to focus on,
  ; and what numbers to try
  (local (
          (define filled-blanks (map (λ (sq) (possibilities sq bd)) ITERATOR))
          (define focus-square (@minimum (n-possible filled-blanks)))
          (define best-guesses (list-ref filled-blanks focus-square))
          (define (square-filler guesses)
            ; [ListOf N] -> [Maybe [ListOf N]]
            ; coordinates the backtracking aspects by filling squares
            ; one-by-onesuch that search stops as soon as a solution
            ; has been identified
            (cond
              [(empty? guesses) #f]
              [(andmap (λ (n) (> n 0)) bd) bd]
              [else
               (local (
                       (define candidate-soln
                         (solve (try (first guesses) focus-square bd))))
                 ; - IN -
                 (if (false? candidate-soln)
                     (square-filler (rest guesses))
                     candidate-soln))]))
          (define (try num sqid board)
            ; N N -> [ListOf N]
            ; tries a solution at the seemingly most solvable square
            (if (= 0 sqid)
                (cons num (rest board))
                (cons (first board) (try num (sub1 sqid) (rest board))))))
    ; - IN -
    (square-filler best-guesses)))


(define (possibilities n board)
  ; N [ListOf N] -> N
  ; derive the list of possible values that can naiively solve square n
  (cond
    [(> (read-square n board) 0) #f]
    [else
     (local (
             (define squares
               (foldr append '() (filter (λ (z) (member? n z)) ZONES)))
             (define impossibilities
               (list-to-set (map (λ (m) (read-square m board)) squares)))
             (define (pare l-obj l-subj)
               (cond
                 [(empty? l-obj) l-subj]
                 [else (pare (rest l-obj) (remove (first l-obj) l-subj))])))
       ; - IN -
       (pare impossibilities INTS))]))


(define (read-square n board)
  ; [ListOf N] N -> N
  ; retrieves the value of the square at position n
  (cond
    [(= 0 n) (first board)]
    [else (read-square (sub1 n) (rest board))]))


(define (n-possible maybe-lst)
  ; [ListOf [Maybe [ListOf N]]] -> N
  ; determines the length of the shortest remaining solution list
  (local (
          (define (len x)
            (cond
              [(false? x) 100] ; need large # in case of solved so to skip it
              [else (length x)])))
    ; - IN -
    (map len maybe-lst)))

            
(define (@minimum lst)
  ; [ListOf N] -> N
  ; identifies the index of the first minimum in the list
  (local (
          (define (leastest lst mn i n)
            ; [ListOf N] N N N -> N
            ; finds mindex by recursion
            (cond
              [(empty? lst) i]
              [(< (first lst) mn) (leastest (rest lst) (first lst) n (add1 n))]
              [else (leastest (rest lst) mn i (add1 n))])))
    ; - IN -
    (leastest lst 10 0 0)))


(define (list-to-set lst)
  ; [ListOf X] -> [ListOf X]
  ; converts a list into a set
  (cond
    [(empty? lst) '()]
    [(member? (first lst) (rest lst)) (list-to-set (rest lst))]
    [else (cons (first lst) (list-to-set (rest lst)))]))


;; !!! refactor this
(define (display board)
  ; [ListOf X] -> Img
  ; displays the state of the sudoku board
  (local (
          (define-struct formatter [pref suff])
          (define (get-row rze n)
            (cond
              [(= n 0) rze]
              [else (get-row
                     (make-formatter
                      (cons (first (formatter-suff rze)) (formatter-pref rze))
                      (rest (formatter-suff rze)))
                     (sub1 n))]))
          (define (board->rows rze)
            (cond 
              [(empty? (formatter-suff rze)) (list (formatter-pref rze))]
              [else (local (
                            (define stuff (get-row rze 9)))
                      (cons (formatter-pref stuff)
                            (board->rows
                             (make-formatter '() (formatter-suff stuff)))))]))
          (define rows (board->rows (make-formatter '() board))))
    ; - IN -
    (foldr above (rectangle 0 0 "solid" "white")
           (map (λ (r) (foldl beside (rectangle 0 0 "solid" "white")
                              (map (λ (n) (overlay (text (number->string n) 24 "green")
                                                   (rectangle 24 24 "solid" "white"))) r))) rows))))



; ============================
; action!

(define sudoku (list 6 0 0 0 0 0 0 0 9
                     0 0 2 1 0 0 0 5 7
                     0 3 0 0 2 0 0 0 0
                     0 0 0 6 0 0 3 0 0
                     0 0 5 0 0 0 9 0 0
                     0 8 0 0 0 7 0 6 5
                     0 0 3 7 0 0 0 1 2
                     4 0 0 0 0 8 0 0 0
                     0 0 0 0 0 0 6 0 0))


(define easydoku (list 2 8 0 0 0 3 0 7 0
                       0 0 0 6 0 0 0 0 9
                       0 5 6 4 0 7 0 0 0
                       0 6 3 1 2 0 7 9 8
                       0 0 0 8 5 0 0 0 4
                       8 1 4 7 3 0 6 5 0
                       0 0 2 0 0 8 0 0 0
                       6 4 0 0 0 0 9 0 3
                       0 0 0 3 0 0 2 6 0))

(display sudoku)
(rectangle 24 24 "solid" "white" )
(display (solve sudoku))