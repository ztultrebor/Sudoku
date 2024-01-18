;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define (read-square n board)
  ; [ListOf N] N -> N
  ; retrieves the value of the square at position n
  (cond
    [(= 0 n) (first board)]
    [else (read-square (sub1 n) (rest board))]))


(define (possibilities n board)
  ; [ListOf N] N -> N
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


(define (inpencil board solns)
  (cond
    [(empty? solns) '()]
    [else (local (
                    (define solvit (first solns)))
              (cons 
               (cond
                 [(false? solvit) (first board)]
                 [(= 1 (length solvit)) (first solvit)]
                 [else (first board)])
               (inpencil (rest board) (rest solns))))]))


(define (difficulty n board)
  ; [ListOf N] N -> N
  ; determines the number of possible values that can naiively solve square n
  (cond
    [(> (read-square n board) 0) #f]
    [else
     (local (
             (define squares
               (foldr append '() (filter (λ (z) (member? n z)) ZONES)))
             (define impossibilities
               (list-to-set (map (λ (m) (read-square m board)) squares))))
       ; - IN -
       (- 10 (length impossibilities)))]))


(define (list-to-set lst)
  ; [ListOf X] -> [ListOf X]
  ; converts a list into a set
  (cond
    [(empty? lst) '()]
    [(member? (first lst) (rest lst)) (list-to-set (rest lst))]
    [else (cons (first lst) (list-to-set (rest lst)))]))


; ============================
; action!

(define sudoku (list 8 0 4 2 0 0 1 5 0
                     3 0 0 0 0 9 0 0 0
                     0 0 0 0 0 0 0 0 8
                     0 5 0 0 2 0 0 0 0
                     0 0 0 0 0 0 0 6 0
                     0 0 1 5 0 0 7 4 0
                     0 0 8 0 0 0 0 0 2
                     4 0 0 0 3 0 8 7 0
                     0 0 0 7 0 0 0 0 6))
(define chalunge (map (λ (sq) (possibilities sq sudoku)) ITERATOR))
chalunge

(define sudoku-1 (inpencil sudoku chalunge))
(define chalunge-1 (map (λ (sq) (possibilities sq sudoku-1)) ITERATOR))
chalunge-1