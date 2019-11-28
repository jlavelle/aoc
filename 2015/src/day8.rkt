#lang racket

(define (read-input) (file->lines "../inputs/day8"))

(define (code-sum strs) (for/sum ([s strs]) (string-length s)))

(define (mem-sum strs)
  (for/sum ([s (map (λ (s) (read (open-input-string s))) strs)])
    (if (equal? eof s) 0 (string-length s))))

(define (enc-sum strs)
  (for/sum ([s strs])
    (string-length (~v s))))

(define (solve1 strs) (- (code-sum strs) (mem-sum strs)))
(define (solve2 strs) (- (enc-sum strs) (code-sum strs)))