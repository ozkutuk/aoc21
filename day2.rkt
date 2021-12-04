#lang racket

(require threading)


; some utility functions

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

(define (make-direction p)
  (let ([direction (car p)]
        [value (cdr p)])
    (cond
      [(eq? direction 'forward) (cons 0 value)]
      [(eq? direction 'up) (cons (- value) 0)]
      [(eq? direction 'down) (cons value 0)]
      [else (cons 0 0)])))

(define (add-pairs p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(define (multiply-pair p)
  (* (car p) (cdr p)))

(define (part1 xs)
  (~>> xs
      (map make-direction)
      (foldl add-pairs (cons 0 0))
      multiply-pair))


; part 2
; acc: (horizontal, depth, aim)

(define (modify-pos f l)
  (list (f (first l)) (second l) (third l)))

(define (increase-pos x l)
  (modify-pos (curry + x) l))

(define (modify-depth f l)
  (list (first l) (f (second l)) (third l)))

(define (increase-depth x l)
  (modify-depth (curry + x) l))

(define (modify-aim f l)
  (list (first l) (second l) (f (third l))))

(define (increase-aim x l)
  (modify-aim (curry + x) l))

(define (decrease-aim x l)
  (modify-aim (lambda (y) (- y x)) l))

(define (step x acc)
  (let ([direction (car x)]
        [value (cdr x)]
        [aim (third acc)])
    (cond
      [(eq? direction 'forward)
         (~>> acc
              (increase-pos value)
              (increase-depth (* aim value)))]
      [(eq? direction 'up) (decrease-aim value acc)]
      [(eq? direction 'down) (increase-aim value acc)])))

(define (mul-pos-depth l)
  (* (first l) (second l)))

(define (part2 xs)
  (~>> xs
       (foldl step (list 0 0 0))
       mul-pos-depth))


; print solutions

(println (part1 input))
(println (part2 input))
