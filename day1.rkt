#lang racket

(define (count-positive-diffs window xs)
  (let ([increments (for/list ([l xs]
                               [r (drop xs window)]
                               #:when (positive? (- r l)))
                               (- r l))])
       (length increments)))

(define part1 ((curry count-positive-diffs) 1))
(define part2 ((curry count-positive-diffs) 3))

(define (solve)
  (let ([input (map string->number (file->lines "input1.txt"))])
    (println (part1 input))
    (println (part2 input))))

(solve)
