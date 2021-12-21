(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)))


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (helper s index lst)
  	(if (null? s) lst
  		(helper (cdr s) (+ index 1) (append lst (list (list index (car s))))))
  	)
  (helper s 0 ()))
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (define (helper comp list1 list2 product)
  (cond
  ((null? list1) (append product list2))
  ((null? list2) (append product list1))
  ((comp (car list1) (car list2)) (helper comp (cdr list1) list2 (append product (list (car list1)))))
  (else (helper comp list1 (cdr list2) (append product (list (car list2)))))))
  (helper comp list1 list2 ()))
  ; END PROBLEM 16

; This also works too
;  (define (merge comp list1 list2)
;    (cond
;    	((null? list1) list2)
;    	((null? list2) list1)
;    	((comp (car list1) (car list2)) (cons (car list1) (merge comp (cdr list1) list2)))
;    	((comp (car list2) (car list1)) (cons (car list2) (merge comp (cdr list2) list1)))
;    	(else) (cons (car list2) (merge comp (cdr list2) list1))


;; Problem 17

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (append (cons form (cons params (cons (car (map let-to-lambda body)) nil))) (cdr (map let-to-lambda body)))
           ; END PROBLEM 17
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (append (cons (cons 'lambda (cons (car (zip values)) (cons (car (map let-to-lambda body)) nil))) nil) (map let-to-lambda (cadr (zip values))))
           ; END PROBLEM 17
           ))
        (else
         ; BEGIN PROBLEM 17
         (cons (car expr) (map let-to-lambda (cdr expr)))
         ; END PROBLEM 17
         )))
