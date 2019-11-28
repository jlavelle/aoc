#lang racket

; TODO Use arrays from the math lib

(define (read-input) (file->lines "../inputs/day6"))

(struct instruction (proc rect) #:transparent)
(struct action-spec (on off toggle) #:transparent)
(struct rect (x1 y1 x2 y2) #:transparent)

(define (parse-line as l)
  (match-define (list* _ action coords)
    (regexp-match #px"^(.*?) (\\d+),(\\d+) through (\\d+),(\\d+)$" l))
  (define proc
    (case action
      [("turn on") (action-spec-on as)]
      [("turn off") (action-spec-off as)]
      [else (action-spec-toggle as)]))
  (instruction proc (apply rect (map string->number coords))))

(define (parse-input-with as in)
  (map (curry parse-line as) in))

(define (in-rect r)
  (for*/stream ([x (in-range (rect-x1 r) (+ 1 (rect-x2 r)))]
                [y (in-range (rect-y1 r) (+ 1 (rect-y2 r)))])
    (cons x y)))

(define (lights w h init instrs)
  (define grid (make-vector (* w h) init))
  (for* ([i instrs]
         [p (in-rect (instruction-rect i))])
    (let* ([idx (flat-index (- h 1) p)]
           [cur (vector-ref grid idx)]
           [fn (instruction-proc i)])
      (vector-set! grid idx (fn cur))))
  grid)

(define (flat-index h p)
  (match p [(cons x y) (+ (* y h) x)]))

(define solve1-spec
  (action-spec
   (const #t)
   (const #f)
   not))

(define solve2-spec
  (action-spec
   add1
   (λ (x) (max (sub1 x) 0))
   (λ (x) (+ x 2))))

(define (solve1)
  (define parsed
    (parse-input-with solve1-spec (read-input)))
  (vector-length
   (vector-filter identity
                  (lights 1000 1000 #f parsed))))

(define (solve2)
  (define parsed
    (parse-input-with solve2-spec (read-input)))
  (for/sum ([x (in-vector (lights 1000 1000 0 parsed))])
    x))