(load "simpleParser.scm")

; undeclared variables have this value
(define undeclared_value
  (lambda ()
    'undeclared))

; unassigned variables have this value
(define error_value
  (lambda ()
    'error))

(define interpret
  (lambda (filename)
    (evaluate_tree (parser filename))))

(define evaluate_tree
  (lambda (tree)
    (search 'return (M_state_stmt-list tree '(()())))))

(define M_state_stmt-list
  (lambda (stmt-list state)
    (if (null? stmt-list)
        state
        (M_state_stmt-list (cdr stmt-list) (M_state_stmt (car stmt-list) state)))))

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

; a helper function: given a nonterminal, determine if it is a <type>
(define type?
  (lambda (nterm)
    (eq? nterm 'var))) 
;one or the other

; a helper function: given the "operative" (the first) element in the stmt's list, call a function
; with the parameters, eg given (= x 1) call the appropriate function with (x 1)
(define call_on_stmt
  (lambda (function stmt state)
    (function (cdr stmt) state)))

; given the whole state, get the list of names and values, respectively
(define get_state_names
  (lambda (state)
    (car state)))

(define get_state_values
  (lambda (state)
    (cadr state)))

; perform a list operation on the state, both names and values are affected alike
(define state_listop
  (lambda (state function)
    (cons (function (get_state_names state)) (cons (function (get_state_values state)) '()))))

; helper function for if the state is empty
(define is_state_empty
  (lambda (state)
    (and (null? (car state)) (null? (cadr state)))))

; adds a variable to the state with the given name and value
(define add_to_state
  (lambda (name value state)
    (cond
      ; if we didn't find a previous value, add it, and if we did, replace it
      ((is_state_empty state) (cons (list name) (list (cons value '()))))
     ; ((feq? (get_state_names state) name) ((cons name (cdr (get_state_names state)))(cons value (cdr (get_state_values state)))))
      ((feq? (get_state_names state) name) (cons (cons name (cdr (car state)))(list(cons value (cdr (car (cdr state)))))))
      ; if we're not at the end yet and haven't found it, recur
      ;(else (cons (car (get_state_names state)) (car(add_to_state name value (state_listop state cdr))) (cadr(cons (car (get_state_values state) (add_to_state name value (state_listop value cdr))))))))))
      (else (integrate (car (get_state_names state)) (car (get_state_values state)) (add_to_state name value (cons (cdr (car state)) (cons (cdr (car (cdr state))) '()))))))))

;
(define integrate
  (lambda (name value state)
    (cons (cons name (car state)) (cons (cons value (car (cdr state))) '()))))
; racket supports short circuit evaluation, so we can write this as one conditional
(define M_state_stmt
  (lambda (nterm state)
    (cond
      ((feq? nterm 'if) (call_on_stmt M_state_if nterm state))
      ((feq? nterm 'while) (call_on_stmt M_state_while nterm state))
      ((type? (car nterm)) (call_on_stmt M_state_declare nterm state))
      ((feq? nterm '=) (call_on_stmt M_state_assign nterm state))
      ((feq? nterm 'return) (call_on_stmt M_state_return nterm state)) ; if we're returning x, it passes the name and not value to M_state_return 
      (else (error (cons "symbol not recognized" (car nterm)))))))

(define has_else?
  (lambda (nterm)
    (eq? (len nterm) 3)))

(define M_state_if
  (lambda (nterm state)
    (cond
      ((eq? (M_boolean_condition (car nterm) state) #t) (M_state_stmt (cadr nterm) state))
      ((has_else? nterm) (M_state_stmt (cddr nterm) state))
      (else state))))

(define M_state_while
  (lambda (nterm state)
    (cond
      ((eq? (M_boolean_condition nterm state) #t) (M_state_while nterm (M_state_stmt (cdr nterm))))
      ;((eq? (car nterm) #t) (M_state_while nterm (M_state_stmt (cdr nterm))))
      (else state))))

(define declare_has_assign?
  (lambda (nterm)
    (eq? (len nterm) 3)))

(define M_state_declare
  (lambda (nterm state)
    (if (declare_has_assign? nterm)
        (add_to_state (car nterm) (cdr nterm) state) ;add the variable to the state and assign it
        (add_to_state (car nterm) (error_value) state)))) ;otherwise just assign it error

(define M_state_assign
  (lambda (nterm state)
    (add_to_state (car nterm) (M_value_plus (cadr nterm) state) state)
    ))

(define M_state_return
  (lambda (nterm state)
    (if (equal? (search nterm state) undeclared_value)
        (add_to_state 'return (M_value_plus (car nterm) state) state)
        (add_to_state 'return (M_value_plus (search (car nterm) state) state) state))))

;returns the value of a pair in the state ex: looking for y in (y 12) returns twleve
(define search
  (lambda (x state)
    (cond
      ((is_state_empty state) undeclared_value)
      ((eq? x (caar state)) (car (car (cdr state))))
      (else (search x (cons (cdr (car state)) (list (cdr (car (cdr state))))))))))
 
(define M_boolean_condition
  (lambda (nterm state)
    (M_boolean_ored_expression nterm state))) ;this would be the conditional

(define M_boolean_ored_expression
  (lambda (nterm state)
    (if (feq? nterm '||)
        (or (M_boolean_ored_expression (cadr nterm) state) (M_boolean_ored_expression (cddr nterm) state))
        (M_boolean_anded_expression nterm state))))

(define M_boolean_anded_expression
  (lambda (nterm state)
    (if (feq? nterm '&&)
        (and (M_boolean_anded_expression (cadr nterm) state) (M_boolean_anded_expression (cddr nterm) state))
        (M_boolean_compare_expression nterm state))))

; helper function for comparing the two parts of an expression
(define compare_value
  (lambda (nterm state function)
    (function (M_value_plus (cadr nterm) state) (M_value_plus (caddr nterm) state))))

(define M_boolean_compare_expression
  (lambda (nterm state)
    (cond
      ((feq? nterm '!) (not (M_boolean_compare_expression (cdr nterm) state)))
      ((feq? nterm '==) (compare_value nterm state eq?))
      ((feq? nterm '!=) (compare_value nterm state (not eq?)))
      ((feq? nterm '<) (compare_value nterm state <))
      ((feq? nterm '>) (compare_value nterm state >))
      ((feq? nterm '<=) (compare_value nterm state <=))
      ((feq? nterm '>=) (compare_value nterm state >=)))))

(define M_value_plus
  (lambda (nterm state)
      (if (feq? nterm '+)
          (+ (M_value_plus (cadr nterm) state) (M_value_plus (caddr nterm) state))
          (M_value_minus nterm state))))

(define M_value_minus
  (lambda (nterm state)
      (if (and (feq? nterm '-) (> (len nterm) 2))
          (- (M_value_plus (cadr nterm) state) (M_value_plus (caddr nterm) state))
          (M_value_times nterm state))))

(define M_value_times
  (lambda (nterm state)
      (if (feq? nterm '*)
          (* (M_value_plus (cadr nterm) state) (M_value_plus (caddr nterm) state))
          (M_value_div nterm state))))

(define M_value_div
  (lambda (nterm state)
      (if (feq? nterm '/)
          (quotient (M_value_plus (cadr nterm) state) (M_value_plus (caddr nterm) state))
          (M_value_mod nterm state))))

(define M_value_mod
  (lambda (nterm state)
      (if (feq? nterm '%)
          (modulo (M_value_plus (cadr nterm) state) (M_value_plus (caddr nterm) state))
          (M_value_negative nterm state))))

(define M_value_negative
  (lambda (nterm state)
    (if (feq? nterm '-)
        (* -1 (M_value_plus (cdr nterm)))
        (M_value_terminal nterm state))))

(define M_value_terminal
  (lambda (term state)
    (cond
      ((list? term) (error (cons "nonterminal at the end of the parse tree" term)))
      ((eq? term "true") #t)
      ((eq? term "false") #f)
      ((not (eq? (search term state) undeclared_value)) (search term state))
      (else term))))
