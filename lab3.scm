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
(define (set? list)
(cond
	;empty list is a set
	((null? list) #t)
	
	;first element in list exists somewhere else in list
	((not (not (member? (car list) (cdr list)))) #f)
	
	;recursively call set? on list without the first element
	(else (set? (cdr list)))
))

;Testing set?:
(display "Testing set?:\n")
(display (set? '(x y z))); −→ #t
(display " should be true\n")
(display (set? '(a 1 b 2 c 3))); −→ #t
(display " should be true\n")
(display (set? '())); −→ #t; empty set is a set
(display " should be true\n")
(display (set? '(a b b c 3))); −→ #f; duplicate, bad set
(display " should be false\n")
(display (set? '(5 9 7 1 5))); −→ #f; duplicate, bad set
(display " should be false\n\n")

;Union function:
(define (union list1 list2)
;CHECK IF VALID INPUT (list1 and list2 must be sets)
;IF NOT VALID, PRINT ERROR MESSAGE, REMOVE DUPLICATES, CALL union ON NEW LISTS

(cond
	;if list2 is null, continue using list1
    ((null? list2) list1)
    
    ;checking if the first element in list2 is in list1  
	((member? (car list2) list1)
	          
	;recursively calling union on list1 and list2 without the first element
    (union list1 (cdr list2)))
   
    ;combine the first element of list2 with list1
    ;recursively call union on this list with list2 without the first element
    (#t (union (cons (car list2) list1) (cdr list2))) 
))

;Intersect function:
(define (intersect list1 list2)
;CHECK IF VALID INPUT (list1 and list2 must be sets)
;IF NOT VALID, PRINT ERROR MESSAGE, REMOVE DUPLICATES, CALL intersect ON NEW LISTS	

;if list1 is null, continue as if it were an empty list
(if (null? list1) '()
    ;checking if the first element in list1 is in list2
    (let ((included (member? (car list1) list2)))
    
    ;if null, continue using list1 without its first element
    (if (null? (cdr list1))
        (if included list1 '())
        (if included
            ;recursively call intersect on list1 without the first element and list2
            ;combine this result with the first element from list1
            (cons (car list1) (intersect (cdr list1) list2))
            
            ;recursively call intersect on list1 without the first element and list2
            (intersect (cdr list1) list2))))	
))

;Lab Questions:
(display "Lab Questions:")

(display "\n1. ")
(display (f '(2 7 8 1 3 9)))
         
(display "\n6. ")
(display (member? 'one '(1 2 3 4 5)))

(display  "\n7. ")
(display (member? 'd '(a b c d c b a)))

(display "\n8. ")
(display (set? '(a b c d c b a)))

(display "\n9. ")
(display (set? '(it was the best of times, it was the worst of times)))

(display "\n10. ")
(display (union '(blue eggs and cheese) '(ham and sandwich)))

(display "\n11. ")
(display (intersect '(blue cheese and cherry) '(cheese and blue eggs)))