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


(define (solve board)
  (local (
          (define solns (map (λ (sq) (possibilities sq sudoku)) ITERATOR))
          (define minimal (n-possible solns))
          (define square-id (@minimum minimal))
          (define minsk (list-ref minimal square-id)))
    ; - IN -
    (cond
      [(= 0 minsk) #f]
      [(solved? board) board]
      [else
       (map (λ (n) (try n square-id board)) (list-ref solns square-id))])))

  
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
              [(false? x) 100]
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
    (leastest lst 10 -1 0)))


(define (try num sq board)
  ; N N [ListOf N] -> [ListOf N]
  ; tries a solution at the seemingly most solvable square
  (cond
    [(empty? board) '()]
    [(= 0 sq) (cons num (rest board))]
    [else (cons (first board) (try num (sub1 sq) (rest board)))]))


(define (list-to-set lst)
  ; [ListOf X] -> [ListOf X]
  ; converts a list into a set
  (cond
    [(empty? lst) '()]
    [(member? (first lst) (rest lst)) (list-to-set (rest lst))]
    [else (cons (first lst) (list-to-set (rest lst)))]))


(define (solved? board)
  (not (ormap (λ (s) (= 0)) board)))


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

(define s1 (first (solve sudoku)))
(define s2 (first (solve s1)))
(define s3 (first (solve s2)))
(define s4 (first (solve s3)))
(map (λ (sq) (possibilities sq s4)) ITERATOR)