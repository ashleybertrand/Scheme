;Ashley Bertrand
;Megan Weller
;Lab 3 - Scheme

;Warmup:
(define (f list)
	; (a) ;
    (if (null? list)
        ; (b) ;
        '()
        ; (c) ;
        (cons (+ 1 (car list)) (f (cdr list)))))

;Member? function:
(define (member? e list)
(cond
	;if list is null, return #f
    ((null? list) #f)
    
    ;if the first element in list is e, return #t
	((eq? e (car list)) #t)
	
	;recursively call member? on list without the first element
    (else (member? e (cdr list)))
))

;Set? function:
;(define (set? list)

;)

;Testing:
;(display (set? (x y z))); −→ #t
;(display (set? (a 1 b 2 c 3))); −→ #t
;(display (set? ())); −→ #t; empty set is a set
;(display (set? (a b b c 3))); −→ #f; duplicate, bad set
;(display (set? (5 9 7 1 5))); −→ #f; duplicate, bad set

;Union function:
;(define (union list1 list2)

;)

;Intersect function:
;(define (intersect list1 list2)
	
;)

;Lab Questions:
(display "Lab Questions:")

(display "\n1. ")
;(display (f '((2 7 8 1 3 9)))
         
(display "\n6. ")
(display (member? 'one '(1 2 3 4 5)))

(display  "\n7. ")
(display (member? 'd '(a b c d c b a)))

(display "\n8. ")
;(display (set? '(a b c d c b a)))

(display "\n9. ")
;(display (set? '(it was the best of times, it was the worst of times)))

(display "\n10. ")
;(display (union '(blue eggs and cheese) '(ham and sandwich)))

(display "\n11. ")
;(display (intersect '(blue cheese and cherry) '(cheese and blue eggs))