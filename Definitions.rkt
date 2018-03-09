; Brennan McFarland
; Lucas Alva

(require racket/trace)

(define error_undeclared_variable
  (lambda (var)
    (error "variable use before declaration: " var)))

(define error_unassigned_variable
  (lambda (var)
    (error "variable use before assignment: " var)))

(define error_unrecognized_symbol
  (lambda (var)
    (error "symbol not recognized: " var)))

(define error_parse_failure
  (lambda (var)
    (error "nonterminal at the end of the parse tree" var)))
    
; undeclared variables have this value
(define undeclared_value
  (lambda ()
    'undeclared))

; unassigned variables have this value
(define error_value
  (lambda ()
    'error))

(define empty_list
  (lambda ()
    '()))

(define empty_state
  (lambda ()
    '((()()))))

(define minus
  (lambda ()
    'minus))

(define negative
  (lambda ()
    'neg))

; a helper function: given a list and S-expression, determine if the first element in the list equals
; the S-expression
(define feq?
  (lambda (lis s)
    (if (and (list? lis) (not(null? lis)))
         (eq? (car lis) s)
         #f)))

(define len
  (lambda (lis)
	(if (null? lis)
            0
            (+ 1 (len (cdr lis))))))