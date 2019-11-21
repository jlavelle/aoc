#lang racket

(define (read-input) (string->list (file->string "../inputs/day3")))

(define (step location dir)
  (match location
    [(cons x y)
     (match dir
       [#\> (cons (add1 x) y)]
       [#\< (cons (sub1 x) y)]
       [#\^ (cons x (add1 y))]
       [#\v (cons x (sub1 y))]
       [_ (cons x y)])]))

(define (santas-route route [santas 1])
  (for/fold ([locations (map (const (cons 0 0)) (range santas))]
             [visited (hash (cons 0 0) santas)])
            ([x route]
             [i (range (length route))])
    (let* ([current-santa
            (modulo i santas)]
           [next-locations
            (list-update locations current-santa (λ (loc) (step loc x)))]
           [next-visited
            (hash-update visited (list-ref next-locations current-santa) add1 0)])
      (values next-locations next-visited))))

(define (solve1) (let-values ([(_ m) (santas-route (read-input))]) (length (hash->list m))))
(define (solve2) (let-values ([(_ m) (santas-route (read-input) 2)]) (length (hash->list m))))
