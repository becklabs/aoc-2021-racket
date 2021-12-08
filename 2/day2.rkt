#lang racket
(require 2htdp/batch-io)
(define INPUT (map (lambda (x) (string-split x " "))(read-lines "input.txt")))

;; Part 1
(define-struct pos [x y])

(define (day2pt1 commands)
(local
    [
     (define (process-commands x y commands)
       (if (empty? commands) (make-pos x y)
           (case (car (first commands))
         [("forward") (process-commands (+ x (string->number (first (cdr (first commands)))))
                                        y
                                        (rest commands))]
         [("down") (process-commands x
                                     (+ y (string->number (first (cdr (first commands)))))
                                      (rest commands))]
         [("up") (process-commands x
                                     (- y (string->number (first (cdr (first commands)))))
                                      (rest commands))])))
     ]
  (process-commands 0 0 commands)))

(define FINAL-POS-1 (day2pt1 INPUT))
(* (pos-x FINAL-POS-1) (pos-y FINAL-POS-1))

;; Part 2
(define-struct pos-2 [x y z])

(define (day2pt2 commands)
(local
    [
     (define (process-commands x y z commands)
       (if (empty? commands) (make-pos-2 x y z)
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

(define FINAL-POS-2 (day2pt2 INPUT))
(* (pos-2-x FINAL-POS-2) (pos-2-y FINAL-POS-2))