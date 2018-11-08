#lang racket

;This is an example implementation of ins_beg,
;It obviously doesn't do what it should, so you
;can edit this function to get started.
;
;Please note the provide function is necessary
;for the unit tests to work. Please include a
;(provide) for each function you write in your
;submitted assignment.
;
;You may delete these comments!

;A)
(provide ins_beg)

(define (ins_beg el lst)
  (display "Output list: ")
  (display (append(list el) lst))
  (display "\nHello, I'm ins_beg!\n"))

(ins_beg '(j a) '(a b n))

;B)
(provide ins_end)

(define (ins_end el lst)
  (display "Output list: ")
  (display (append lst (list el)))
  (display "\nHello, I'm ins_end\n"))

(ins_end '(j a) '(a b n))

;C)
(display "\ncout_top_level:\n")
(provide cout_top_level)

(define (cout_top_level ctllist)
  (display (length ctllist)))

(cout_top_level '(a (1 2 3) "el"))

;D)
(display "\ncount_instances:\n")
(provide count_instances)

(define (count_instances lst el)
  (cond
    [(null? lst) 0]
    [(eq? el (car lst)) (+ 1 (count_instances (cdr lst) el))]
    [else (count_instances (cdr lst) el)]))
(display (count_instances '(1 3 3 3 4) 3))

;E)
(display "\ntail_count:\n")
(provide help_count)
(provide tail_count)

(define (help_count lst el)
  (tail_count lst el 0))
(define (tail_count lst el cnt)
  (cond
    [(null? lst) cnt]
    [(eq? el (car lst)) (tail_count (cdr lst) el (+ 1 cnt))]
    [else (tail_count (cdr lst) el cnt)]
  )
)
(display (help_count '(1 3 s 3 4) 3))

;F)
(display "\ncount_instances_deep:\n")
(provide atom?)
(provide help_deep)
(provide count_instances_deep)

(define (atom? x) (not (or (pair? x) (null? x))))
(define (help_deep lst el)
  (count_instances_deep lst el 0))
(define (count_instances_deep lst el cnt) 
  
  (cond
    [(null? lst) cnt]

    [(atom? (car lst))
     (cond
    
       [(eq? el (car lst)) (count_instances_deep (cdr lst) el (+ 1 cnt))]
    
       [else (count_instances_deep (cdr lst) el cnt)]
       )]
    [else (+ (+ cnt (count_instances_deep (car lst) el 0)) (count_instances_deep (cdr lst) el 0))]
  )
 )
(display (help_deep '((1 3) 3 3 a s s s s ( f s ) (4 3 3 0) 3) 's))