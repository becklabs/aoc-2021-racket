#lang racket
(require 2htdp/batch-io)
(define INPUT (map (lambda (x) (string-split x " "))(read-lines "input.txt")))

(define-struct pos [x y z])

(define (day2 commands)
(local
    [
     (define (process-commands x y z commands)
       (if (empty? commands) (make-pos x y z)
           (case (car (first commands))
             [("forward") (process-commands (+ x (string->number (first (cdr (first commands)))))
                                            (+ y (* z (string->number (first (cdr (first commands))))))
                                            z
                                            (rest commands))]
             [else (process-commands x y
                                     ((if (string=? (car (first commands)) "down") + -)
                                      z (string->number (first (cdr (first commands)))))
                                     (rest commands))])))
     ]
  (process-commands 0 0 0 commands)))


(define FINAL-POS (day2 INPUT))

;; Part 1
(* (pos-x FINAL-POS) (pos-z FINAL-POS))

;; Part 2
(* (pos-x FINAL-POS) (pos-y FINAL-POS-2))

