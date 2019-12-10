#lang racket

; TODO This needs to be heavily optimized somehow

(define input 1321131112)

(define (int->digits n)
  (define-values (x _)
    (for/fold ([ds '()]
               [quot #f])
              ([_ (in-naturals)])
       #:break (equal? quot 0)
       (let-values ([(q r) (quotient/remainder (if (not quot) n quot) 10)])
         (values (cons r ds) q))))
    x)

(define (digits->int ds)
  (for/fold ([n 0])
            ([d (reverse ds)]
             [i (in-naturals)])
    (+ n (* d (expt 10 i)))))

(define (eq-spans xs)
  (define-values (ss f)
    (for/foldr ([r '()]
                [sg '()])
               ([x xs])
    (cond
      [(empty? sg) (values r (cons x sg))]
      [(equal? x (first sg)) (values r (cons x sg))]
      [else (values (cons sg r) (list x))])))
  (cons f ss))

(define (look-say ds)
  (define spans (eq-spans ds))
  (for/foldr ([r '()])
             ([g spans])
    (list* (length g) (first g) r)))

(define (iterN n proc a)
  (for/fold ([x a])
            ([_ (in-range n)])
    (proc x)))

(define (solve1) (length (iterN 40 look-say (int->digits input))))
(define (solve2) (length (iterN 50 look-say (int->digits input))))