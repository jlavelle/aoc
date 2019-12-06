#lang racket

(define (read-input) (string->list (file->string "../inputs/day1")))

(define (climb-floor acc x)
  (cond [(eq? x #\() (+ acc 1)]
        [(eq? x #\)) (- acc 1)]
        [else acc]))

(define (enter-basement s p b)
  (if (and (= s -1) (not b)) p b))

(define (count-floors input)
  (for/fold ([sum 0]
             [basement-pos #f])
            ([x input]
             [i (range 1 (add1 (length input)))])
    (let* ([ns (climb-floor sum x)]
           [bp (enter-basement ns i basement-pos)])
      (values ns bp))))

(define (solve1) (let-values ([(a b) (count-floors (read-input))]) a))
(define (solve2) (let-values ([(a b) (count-floors (read-input))]) b))