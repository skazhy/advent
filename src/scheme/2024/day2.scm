;;; Advent of Code 2024, day 2: Red-Nosed Reports
;;; https://adventofcode.com/2024/day/2

(import (chicken string))

(load "src/scheme/helpers.scm")

(define (increasing l)
  (or (< (length l) 2)
      (and (< 0 (- (cadr l) (car l)) 4) (increasing (cdr l)))))

(define (append-end l el)
  (if (null? l)
      (list el)
      (cons (car l) (append-end (cdr l) el))))

(define (is-safe l)
  (if (< (car l) (cadr l))
      (increasing l)
      (increasing (reverse l))))

(define (count-matches m ls)
  (cond
   ((null? ls) 0)
   ((m (car ls)) (+ 1 (count-matches m (cdr ls))))
   (else (count-matches m (cdr ls)))))

(define (is-safe-sublist head tail)
  (cond
   ((null? tail) #f)
   ((is-safe (append head (cdr tail))) #t)
   (else (is-safe-sublist (append-end head (car tail)) (cdr tail)))))

(define (main args)
  (let ((input (map (lambda (r) (map string->number (string-split r)))
                    (read-file (car args)))))
    (display (count-matches is-safe input))
    (newline)
    (display (count-matches (lambda (t) (is-safe-sublist '() t)) input))
    (newline)))
