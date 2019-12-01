#lang racket

(define (read-input) (file->lines "../inputs/day9"))

(define (parse-line str)
  (match (string-split str)
    [(list c1 _ c2 _ dist) (list (set c1 c2) (string->number dist))]
    [_ #f]))

(define (parse-input strs)
  (foldl
   (λ (s h) (apply (curry hash-set h) (parse-line s)))
   (hash)
   strs))

(define (unique-cities city-dists)
  (set->list
   (apply set-union (hash-keys city-dists))))

(define (pair-with-next xs)
  (if (empty? xs) '() 
      (for/list ([x xs]
                 [y (drop xs 1)])
        (set x y))))

(define (tour-length tour city-dists)
  (for/sum ([leg (pair-with-next tour)])
    (hash-ref city-dists leg)))

(define (best-tour-by compare dists)
  (define cities (unique-cities dists))
  (for/fold ([current-min #f])
            ([tour (in-permutations cities)])
    (define len (tour-length tour dists))
    (cond
      [(not current-min) len]
      [(compare current-min len) len]
      [else current-min])))

(define (solve1) (best-tour-by > (parse-input (read-input))))
(define (solve2) (best-tour-by < (parse-input (read-input))))