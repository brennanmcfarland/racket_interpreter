(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (evaluate_tree (parser filename))))

(define evaluate_tree
  (lambda (tree)
    (M_state_stmt-list tree '(()()))))

(define M_state_stmt-list
  (lambda (stmt-list state)
    (if (null? stmt-list)
        state
        (M_state_stmt-list (cdr stmt-list) (M_state_stmt (car stmt-list) state)))))

; a helper function: given a list and S-expression, determine if the first element in the list equals
; the S-expression
(define feq?
  (lambda (lis s)
    (eq? (car lis) s)))

(define len
  (lambda (lis)
	(if (null? lis)
            0
            (+ 1 (len (cdr lis))))))

; a helper function: given a nonterminal, determine if it is a <type>
(define type?
  (lambda (nterm)
    (eq? nterm 'var))) ;TODO: this is a slight shortcut from the BNF, should probably change
;one or the other

; a helper function: given the "operative" (the first) element in the stmt's list, call a function
; with the parameters, eg given (= x 1) call the appropriate function with (x 1)
(define call_on_stmt
  (lambda (function stmt state)
    (function (cdr stmt) state)))

; racket supports short circuit evaluation, so we can write this as one conditional
(define M_state_stmt
  (lambda (nterm state)
    (cond
      ((feq? nterm 'if) (call_on_stmt M_state_if nterm state))
      ((feq? nterm 'while) (call_on_stmt M_state_while nterm state))
      ((type? (car nterm)) (call_on_stmt M_state_declare nterm state))
      ((feq? nterm '=) (call_on_stmt M_state_assign nterm state))
      ((feq? nterm 'return) (call_on_stmt M_state_return nterm state)) ;TODO: forgot about this in the BNF!
      (else error))))

(define has_else?
  (lambda (nterm)
    (eq? (len nterm) 3)))

(define M_state_if
  (lambda (nterm state)
    (cond
      ((eq? (M_value_condition (car nterm) state) #t) (M_state_stmt (cadr nterm) state))
      ((has_else? nterm) (M_state_stmt (cddr nterm) state))
      (else state))))

(define M_state_while
  (lambda (nterm state)
    ((eq? (car nterm) #t) (M_state_while nterm (M_state_stmt (cdr nterm))))
    (else state)))

(define declare_has_assign?
  (lambda (nterm)
    (eq? (len nterm) 3)))

(define M_state_declare
  (lambda (nterm state)
    (if (declare_has_assign nterm)
        () ;add the variable to the state and assign it
        () ;otherwise just assign it
    )))

(define M_state_assign
  (lambda (nterm state)
    state ;TODO
    ))

(define M_state_return
  (lambda (nterm state)
    state ;TODO
    ))

(define M_value_condition
  (lambda (nterm state)
    state ;TODO
    ))