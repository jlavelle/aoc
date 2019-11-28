#lang racket

(define (read-input) (file->lines "../inputs/day8"))

(define (mem-vs-code-len strs)
  (define code-sum (for/sum ([s strs]) (string-length s)))
  (define mem-sum
    (for/sum ([s (map (λ (s) (read (open-input-string s))) strs)])
      (if (equal? eof s) 0 (string-length s))))
  (values mem-sum code-sum))

(define (solve1 strs) (let-values ([(m c) (mem-vs-code-len strs)]) (- c m)))