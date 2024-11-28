;;; Advent of Code 2015, day 1: Not Quite Lisp
;;; https://adventofcode.com/2015/day/1

(load "src/scheme/helpers.scm")

(define (floor-movement bracket)
  (case bracket
    ((#\() 1)
    ((#\)) -1)))

(define (reduce-floors input floor)
  (if (null? input)
      floor
      (reduce-floors (cdr input) (+ floor (floor-movement (car input))))))

(define (basement-idx input floor pos)
  (let ((new-floor (+ floor (floor-movement (car input)))))
    (if (neg? new-floor 0)
        pos
        (basement-idx (cdr input) new-floor (+ pos 1)))))

(define (main args)
  (let ((input (string->list (car (read-file (car args))))))
    (display (reduce-floors input 0))
    (newline)
    (display (basement-idx input 0 1))
    (newline)))
