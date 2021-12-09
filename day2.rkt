#lang racket

(require threading)
(require srfi/26)


; some helper functions

(define (apply-left f)
  (lambda (p)
    (cons (f (car p)) (cdr p))))

(define (apply-right f)
  (lambda (p)
    (cons (car p) (f (cdr p)))))

(define list->pair (apply-right first))


; parse the input into the form: (symbol . number)

(define input
  (~>> "input2.txt"
       file->lines
       (map (compose1 (apply-right string->number)
                      (apply-left string->symbol)
                      list->pair
                      string-split))))


; part 1

(define (step1 x acc)
  (let ([direction (car x)]
        [value (cdr x)])
    (cond
      [(eq? direction 'forward)
         (hash-update acc 'horizontal (curry + value))]
      [(eq? direction 'up)
         (hash-update acc 'vertical (cut - <> value))]
      [(eq? direction 'down)
         (hash-update acc 'vertical (curry + value))])))

(define (add-pairs p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(define (multiply-dimensions h)
  (apply * (hash-values h)))

(define (part1 xs)
  (~>> xs
      (foldl step1 (hasheq 'horizontal 0 'vertical 0))
      multiply-dimensions))


; part 2

(define (step2 x acc)
  (let ([direction (car x)]
        [value (cdr x)]
        [aim (hash-ref acc 'aim)])
    (cond
      [(eq? direction 'forward)
         (~> (hash-update acc 'pos (curry + value))
             (hash-update 'depth (curry + (* aim value))))]
      [(eq? direction 'up)
         (hash-update acc 'aim (cut - <> value))]
      [(eq? direction 'down)
         (hash-update acc 'aim (curry + value))])))

(define (mul-pos-depth h)
  (* (hash-ref h 'pos) (hash-ref h 'depth)))

(define (part2 xs)
  (~>> xs
       (foldl step2 (hasheq 'pos 0 'depth 0 'aim 0))
       mul-pos-depth))


; print solutions

(println (part1 input))
(println (part2 input))
