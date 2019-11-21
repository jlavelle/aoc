#lang racket

(define (read-input) (file->lines "../inputs/day5"))

(define mean-strings (list "ab" "cd" "pq" "xy"))

(define vowels (list->set (string->list "aeiou")))

(define (vowel? char) (set-member? vowels char))

(define (num-vowels str)
  (count vowel? (string->list str)))

; Does an element of xs have a duplicate n spaces ahead of itself?
; Int -> List a -> Bool
(define (has-duplicate? n xs)
  (and (> (length xs) n)
       (for/or ([x1 xs]
                [x2 (drop xs n)])
         (equal? x1 x2))))

(define (pair-without-overlap? xs)
  (and (> (length xs) 2)
       (let* ([pairs (for/list ([x1 xs] [x2 (drop xs 1)]) (cons x1 x2))]
              [non-overlapping? (not (has-duplicate? 1 pairs))])
         (and non-overlapping?
              (let ([freqs (for/fold ([seen (hash)])
                                     ([p pairs])
                             (hash-update seen p add1 0))])
                (> (count (λ (x) (> x 1)) (hash-values freqs)) 0))))))

(define (has-mean-string? str)
  (< 0 (count (λ (x) (string-contains? str x)) mean-strings)))

(define (nice1? str)
  (and
   (>= (num-vowels str) 3)
   (has-duplicate? 1 (string->list str))
   (not (has-mean-string? str))))

(define (nice2? str)
  (and
   (pair-without-overlap? (string->list str))
   (has-duplicate? 2 (string->list str))))

(define (solve1) (count nice1? (read-input)))
(define (solve2) (count nice2? (read-input)))