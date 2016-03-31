;Ashley Bertrand
;Megan Weller
;Lab 3

(display "does this even work?\n")

(define (f list)
        ; (a) ;
        (if (null? list)
                ; (b) ;
                '()
                ; (c) ;
                (cons (+ 1 (car list)) (f (cdr list)))))

;Member? function:
DEFINE (member? e list)
(COND
        ((NULL? list) #F)
        ((EQ? e (CAR list)) #T)
        ((ELSE (member e (CDR list)))
))

