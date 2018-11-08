#lang racket


;A)
(display "\nA)\n")
(provide print-btree)

(define (print-btree tree)
  (cond
    [(null? tree) (display "")]
    [else 
           (print-btree (cadr tree))
           (printf "~a " (car tree)) 
           (print-btree (caddr tree))
           ]
  )
 )
(print-btree '(15 (2 () ()) (18 (16 () ()) (20 () (21 () ())))))

;B)
(display "\nB)\n")
(provide search-btree)

(define (search-btree tree element)
  ;(printf "~a~n" tree )
  (cond
    [(null? tree) #f]
    [(= element (car tree)) #t]
    [(< element (car tree)) (search-btree (cadr tree) element)]
    [(> element (car tree)) (search-btree (caddr tree) element)]
    
  )
 )
(search-btree '(15 (2 () ()) (18 (16 () ()) (20 () (21 () ())))) 2)

;C)
(display "\nC)\n")
(provide insert-btree)

(define (insert-btree tree el)
  (cond
    [(null? el) (tree)]
    [(null? tree) (list el '() '())]
    [(= el (car tree)) tree]
    [(< el (car tree)) (list (car tree) (insert-btree (cadr tree) el) (caddr tree))]
    [(> el (car tree)) (list (car tree) (cadr tree) (insert-btree (caddr tree) el))]
  )
)
(print-btree (insert-btree '(15
                (2 () ()) (18
                           (16 () ()) (20
                                       () (21 () ())))) 18))

;D)
(display "\nD)\n")
(provide insert-list-btree)

(define (insert-list-btree tree lst)
  (if (null? lst)
      tree
      (insert-list-btree (insert-btree tree (car lst)) (cdr lst))
      )
  )
(print-btree (insert-list-btree '(15
               (2 () ()) (18
                           (16 () ()) (20
                                       () (21 () ())))) '(111 1)))

;E)
(display "\nE)\n")
(provide tree-sort)

(define (tree-sort lst)
  (print-btree (insert-list-btree '() lst)))
(tree-sort '(91 16 0 3 20 9 6))


;F)
(display "\nF)\n")

;print in descending order
(provide print-btree-d)
(define (print-btree-d tree)
  (cond
    [(null? tree) (display "")]
    [else 
           (print-btree-d (caddr tree))
           (printf "~a " (car tree)) 
           (print-btree-d (cadr tree))
           ]
  )
 )


(provide tree-sort+)
(define (tree-sort+ lst sort-f)
  (sort-f lst)
  )

(provide asc-sort)
(provide desc-sort)
(provide insert-btree)
(define (asc-sort tree)
  (tree-sort tree))
(define (desc-sort tree)
  (print-btree-d (insert-list-btree '() tree)))
;(define (lastd-sort lst)
 ;)

(tree-sort+ '(91 16 0 3 20 9 6) asc-sort)
(display "\n")
(tree-sort+ '(91 16 0 3 20 9 6) desc-sort)


