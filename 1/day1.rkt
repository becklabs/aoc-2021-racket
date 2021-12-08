#lang racket
(require 2htdp/batch-io)
(define LINES (map string->number (read-lines "input.txt")))

(define (sum-n-elements lox n)
  (if (zero? n) 0
      (+ (first lox) (sum-n-elements (rest lox) (sub1 n)))))

(define (day1 lon n)
  (local
    [
     (define (loop acc lon)
       (if (< (length lon) (+ n 1))  
           acc
           (loop (if (< (sum-n-elements lon n)
                        (sum-n-elements (rest lon) n))
                     (add1 acc) acc)
                 (rest lon))))
     ]
      (loop 0 lon)))

;; Part 1
(day1 LINES 1)

;; Part 2
(day1 LINES 3)


