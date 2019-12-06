#lang racket

(define (read-input) (file->lines "../inputs/day2"))

(define (parse inp)
  (filter-map
   (λ (x)
     (let ([dims (string-split x "x")])
       (if (empty? dims)
           #f
           (map string->number dims))))
   inp))

(define (smallest-side l w h) (take (sort (list l w h) < ) 2))

(define (surface-area l w h) (+ (* 2 l w) (* 2 w h) (* 2 h l)))

(define volume *)

(define (smallest-side-area l w h) (apply * (smallest-side l w h)))

(define (smallest-side-perimeter l w h) (* 2 (apply + (smallest-side l w h))))

(define (required-paper l w h) (+ (surface-area l w h) (smallest-side-area l w h)))

(define (required-ribbon l w h) (+ (volume l w h) (smallest-side-perimeter l w h)))

(define (solve1) (apply + (map (λ (x) (apply required-paper x)) (parse (read-input)))))
(define (solve2) (apply + (map (λ (x) (apply required-ribbon x)) (parse (read-input)))))