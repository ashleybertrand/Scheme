;Ashley Bertrand
;Megan Weller
;Lab 3 - Scheme

;Warmup:
(define (f list)
	;a check to see if list is null
    (if (null? list)
        ;if list is null, continue as if it were an empty list
        '()
        ;Build a list of the current list, adding 1 to the first element, combined with the list recursively called on f without the first element
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
	;For a valid input, list1 and list2 must be sets
	;If not valid, print error message, remove duplicates, call union on new lists
	;list1 contains duplicates
	(if (not (valid? list1))
	    ;print error message until list1 is a set
		(display "List1 is not a valid set. Duplicates will be removed and intersect will be called on the re-defined list.\n")
	)

	;list2 contains duplicates
	(if (not (valid? list2))
	    ;print error message until list2 is a set
		(display "List2 is not a valid set. Duplicates will be removed and intersect will be called on the re-defined list.\n")
	)

	;re-lable list1 to new_list1 and list2 to new_list2 where duplicates are removed if they existed
	(let ((new_list1 (delete list1)))
    	(let ((new_list2 (delete list2)))
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
			)
    	)
	)
)

;Intersect function:
(define (intersect list1 list2)
	;For a valid input, list1 and list2 must be sets
	;If not valid, print error message, remove duplicates, call intersect on new lists
	;list1 contains duplicates
	(if (not (valid? list1))
	    ;print error message until list1 is a set
		(display "List1 is not a valid set. Duplicates will be removed and intersect will be called on the re-defined list.\n")
	)

	;list2 contains duplicates
	(if (not (valid? list2))
	    ;print error message until list2 is a set
		(display "List2 is not a valid set. Duplicates will be removed and intersect will be called on the re-defined list.\n")
	)

	;re-lable list1 to new_list1 and list2 to new_list2 where duplicates are removed if they existed
	(let ((new_list1 (delete list1)))
    	(let ((new_list2 (delete list2)))
    		;if list1 is null, continue as if it were an empty list
			(if (null? new_list1) '()
    			;checking if the first element in list1 is in list2
    			(let ((included (member? (car new_list1) new_list2)))
    
    			;if null, continue using list1 without its first element
    			(if (null? (cdr new_list1))
        			(if included new_list1 '())
        			(if included
            			;recursively call intersect on list1 without the first element and list2
            			;combine this result with the first element from list1
            			(cons (car new_list1) (intersect (cdr new_list1) new_list2))
            
            			;recursively call intersect on list1 without the first element and list2
            			(intersect (cdr new_list1) new_list2))))	
			)
		)	
	)
)

;checks if the provided list is a valid set
(define (valid? list)
(cond
	;if list is null, return true
    ((null? list) #t)
    
    ;checking to see if list is a set
    ((set? list) list)
    
    ;list contains duplicates and is therefore not a set
    (else (display "Invalid input.\n") #f)
))

;removes duplicates from a list to form a set
(define (delete list)
(cond
	;if list is null, continue with empty list
    ((null? list) '() )
    
    ;checking to see if the first element in list is in list somewhere later on
    ;recursively call delete on list without the first element of list
    ((member? (car list) (cdr list)) (delete(cdr list)))
    
    ;recursively call delete on list without the first element of list
    ;combine this result with the first element  of list
    (else (cons (car list) (delete (cdr list))))
))

(define (valid? list)
(cond
    ((null? list) #t)
    
    ((set? list) list)
    
    (else (display "\n Error. Duplicate in set, will redo set\n") #f)
        
))

(define (delete list)
(cond
    ((null? list) '() )
    
    ((member? (car list) (cdr list)) (delete(cdr list)))
    
    (else (cons (car list) (delete (cdr list))))
    
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