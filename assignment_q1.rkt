#lang racket

(cons 1 2)

(cons 1 (cons 2  (cons 3 empty)))

(cons "string" (cons 1 (cons '(1 2 3) empty)))

(list "string" 1 '(1 2 3))

(append '("string") '(1) '((1 2 3)))


