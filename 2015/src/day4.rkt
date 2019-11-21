#lang racket

(require openssl/md5)

; TODO this is quite slow, can we speed it up?

(define input "ckczppom")

(define (hash i n)
  (md5 (open-input-string (string-append i (number->string n)))))

(define (leading-zeros-hash? h n)
  (foldl (λ (a b) (and a b)) #t (map (λ (x) (eq? x #\0)) (take (string->list h) n))))

(define (zero-hashes num-zeroes)
  (for/stream ([n (in-naturals)]
               #:when (leading-zeros-hash? (hash input n) num-zeroes))
    n))

(define (solve1) (stream-first (zero-hashes 5)))
(define (solve2) (stream-first (zero-hashes 6)))