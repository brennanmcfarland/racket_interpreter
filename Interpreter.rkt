; Brennan McFarland
; Lucas Alva

; TODO: the continuation functions are probably being passed around to a lot of functions where they're not used,
; clean that up so they're only passed when they need to be
(require racket/trace)

; other files need to be loaded only once, so we may as well do so here (order matters, first loads first)
(load "Definitions.rkt")
(load "State.rkt")
(load "Test.rkt")
(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (evaluate_tree (parser filename))))

(define evaluate_tree
  (lambda (tree)
    (printval
     (call/cc
      (lambda (return)
        (M_state_stmt-list tree (init_state) return (lambda () ()) (lambda () ()) (lambda () ()))))))) ; the lambdas are to be overwritten

; converts a value from internal representation to what it is displayed as to the user
(define printval
  (lambda (value)
    (cond
      ((eq? value #t) 'true)
      ((eq? value #f) 'false)
      (else value))))

(define M_state_stmt-list
  (lambda (stmt-list state return break continue throw)
    (if (null? stmt-list)
        state
        (M_state_stmt-list (remaining_stmts stmt-list) (M_state_stmt (next_stmt stmt-list) state return break continue throw) return break continue throw))))

; helper functions for getting the next and subsequent statements in a given statement list
(define next_stmt
  (lambda (stmt-list)
    (car stmt-list)))

(define remaining_stmts
  (lambda (stmt-list)
    (cdr stmt-list)))

; a helper function: given a nonterminal, determine if it is a <type>
(define type?
  (lambda (nterm)
    (eq? nterm 'var))) 

; a helper function: given the "operative" (the first) element in the stmt's list, call a function
; with the parameters, eg given (= x 1) call the appropriate function with (x 1)
(define call_on_stmt
  (lambda (function stmt state return break continue throw)
    (function (cdr stmt) state return break continue throw)))

; racket supports short circuit evaluation, so we can write this as one conditional
(define M_state_stmt
  (lambda (nterm state return break continue throw)
    (cond
      ((nterm_oftype? nterm 'if) (call_on_stmt M_state_if nterm state return break continue throw))
      ((nterm_oftype? nterm 'while) (call_on_stmt M_state_while nterm state return break continue throw))
      ((nterm_oftype? nterm 'var) (call_on_stmt M_state_declare nterm state return break continue throw))
      ((nterm_oftype? nterm '=) (call_on_stmt M_state_assign nterm state return break continue throw))
      ; return is an M_value function even though called from an M_state because the continuation breaks normal control flow
      ((nterm_oftype? nterm 'return) (call_on_stmt M_value_return nterm state return break continue throw)) ; if we're returning x, it passes the name and not value to M_value_return
      ((nterm_oftype? nterm 'break) (call_on_stmt M_value_break nterm state return break continue throw))
      ((nterm_oftype? nterm 'continue) (call_on_stmt M_value_continue nterm state return break continue throw))
      ((nterm_oftype? nterm 'throw) (call_on_stmt M_value_throw nterm state return break continue throw))
      ((nterm_oftype? nterm 'try) (call_on_stmt M_state_try nterm state return break continue throw))
      ((nterm_oftype? nterm 'begin) (call_on_stmt M_state_block nterm state return break continue throw)) 
      (else (error_unrecognized_symbol (next_stmt nterm))))))

; determine if the nonterminal is of a given type
(define nterm_oftype?
  (lambda (nterm type)
    (cond
      ((and (feq? nterm '-) (> (len nterm) 2)) (eq? type (minus)))
      ((feq? nterm '-) (eq? type (negative)))
      (else (feq? nterm type)))))

(define has_else?
  (lambda (nterm)
    (eq? (len nterm) 3)))

; a scoping block, ie, brackets
(define M_state_block
  (lambda (nterm state return break continue throw)
       (pop_frame (M_state_stmt-list nterm (push_frame state) return break continue throw))))

(define M_state_if
  (lambda (nterm state return break continue throw)
    (cond
      ((eq? (M_boolean_condition (condition nterm) state) #t) (M_state_stmt (then_stmt nterm) state return break continue throw))
      ((has_else? nterm) (M_state_stmt (else_stmt nterm) state return break continue throw))
      (else state))))

; given a conditional, extract the condition
(define condition
  (lambda (conditional)
    (car conditional)))

; given a conditional, extract the statement to run if the condition is true
(define then_stmt
  (lambda (conditional)
    (cadr conditional)))

; given a conditional with an else, extract the statement to run if the condition is false
(define else_stmt
  (lambda (conditional)
    (caddr conditional)))

(define M_state_while
  (lambda (nterm state return break continue throw)
    (call/cc
     (lambda (break)
       (cond
         ((eq? (M_boolean_condition (condition nterm) state) #t) 
             (M_state_while nterm 
                            (call/cc
                             (lambda (continue)
                               (M_state_stmt (then_stmt nterm) state return break continue throw))) return break continue throw))
         (else state))))))

(define M_state_try
  (lambda (nterm state return break continue throw)
    (call/cc
     (lambda (throw)
       (M_state_stmt-list (try_body_contents nterm) state return break continue throw)))))

(define try_body_contents
  (lambda (body)
    (car body)))
    
; gets if the declare statement also assigns a value
(define declare_has_assign?
  (lambda (nterm)
    (eq? (len nterm) 2)))

(define M_state_declare
  (lambda (nterm state return break continue throw)
    (if (declare_has_assign? nterm)
        (update_state (stmt_var_name nterm) (M_boolean_compare_expression (stmt_var_value nterm) state) state) ;add the variable to the state and assign it
        (update_state (stmt_var_name nterm) (error_value) state)))) ;otherwise just assign it error

; given a variable-modifying statement, extract the variable name
(define stmt_var_name
  (lambda (nterm)
    (car nterm)))

; given a variable-modifying statement with an assignment, extract the value to assign
(define stmt_var_value
  (lambda (nterm)
    (cadr nterm)))

(define M_state_assign
  (lambda (nterm state return break continue throw)
    (if (state_contains? (stmt_var_name nterm) state)
        (update_state (stmt_var_name nterm) (M_boolean_compare_expression (stmt_var_value nterm) state) state)
        ; checking for undeclaration
        (error_undeclared_variable (stmt_var_name nterm)))))

(define M_value_return
  (lambda (nterm state return break continue throw)
    (return (M_boolean_condition (stmt_var_name nterm) state))))

(define M_value_break
  (lambda (nterm state return break continue throw)
    (break state)))

(define M_value_continue
  (lambda (nterm state return break continue throw)
    (continue state)))

(define M_value_throw
  (lambda (nterm state return break continue throw)
    (throw state)))

(trace M_value_throw)

; evaluates a conditional as true or false

(define M_boolean_condition
  (lambda (nterm state)
    (M_boolean_ored_expression nterm state))) ;this would be the conditional

; given a binary operator, get the first/second of the two things being operated on
(define l_operand
  (lambda (operation)
    (cadr operation)))

(define r_operand
  (lambda (operation)
    (caddr operation)))

(define M_boolean_ored_expression
  (lambda (nterm state)
    (if (nterm_oftype? nterm '||)
        (or (M_boolean_ored_expression (l_operand nterm) state) (M_boolean_ored_expression (r_operand nterm) state))
        (M_boolean_anded_expression nterm state))))

(define M_boolean_anded_expression
  (lambda (nterm state)
    (if (nterm_oftype? nterm '&&)
        (and (M_boolean_anded_expression (l_operand nterm) state) (M_boolean_anded_expression (r_operand nterm) state))
        (M_boolean_compare_expression nterm state))))

; helper function for comparing the two parts of an expression
(define compare_value
  (lambda (nterm state function)
    (function (M_boolean_compare_expression (l_operand nterm) state) (M_value_plus (r_operand nterm) state))))

(define M_boolean_compare_expression
  (lambda (nterm state)
    (cond
      ((nterm_oftype? nterm '!) (not (M_boolean_compare_expression (cadr nterm) state))) ;issues with this one and test 18
      ((nterm_oftype? nterm '==) (compare_value nterm state eq?))
      ((nterm_oftype? nterm '!=) (compare_value nterm state (lambda (x y) (not (equal? x y)))))
      ((nterm_oftype? nterm '<) (compare_value nterm state <))
      ((nterm_oftype? nterm '>) (compare_value nterm state >))
      ((nterm_oftype? nterm '<=) (compare_value nterm state <=))
      ((nterm_oftype? nterm '>=) (compare_value nterm state >=))
      (else (M_value_plus nterm state)))))

(define M_value_plus
  (lambda (nterm state)
      (if (nterm_oftype? nterm '+)
          (+ (M_value_plus (cadr nterm) state) (M_value_plus (caddr nterm) state))
          (M_value_minus nterm state))))

(define M_value_minus
  (lambda (nterm state)
      (if (nterm_oftype? nterm (minus))
          (- (M_value_plus (l_operand nterm) state) (M_value_plus (r_operand nterm) state))
          (M_value_times nterm state))))

(define M_value_times
  (lambda (nterm state)
      (if (nterm_oftype? nterm '*)
          (* (M_value_plus (l_operand nterm) state) (M_value_plus (r_operand nterm) state))
          (M_value_div nterm state))))

(define M_value_div
  (lambda (nterm state)
      (if (nterm_oftype? nterm '/)
          (quotient (M_value_plus (l_operand nterm) state) (M_value_plus (r_operand nterm) state))
          (M_value_mod nterm state))))

(define M_value_mod
  (lambda (nterm state)
      (if (nterm_oftype? nterm '%)
          (modulo (M_value_plus (l_operand nterm) state) (M_value_plus (r_operand nterm) state)) 
          (M_value_neg nterm state))))

(define M_value_neg
  (lambda (nterm state)
    (if (nterm_oftype? nterm (negative))
        (* -1 (M_value_plus (l_operand nterm) state))
        (M_value_term nterm state))))

(define M_value_term
  (lambda (term state)
    (cond
      ((list? term) (error_parse_failure term))
      ((eq? term 'true) #t)
      ((eq? term 'false) #f)
      ((number? term) term)
      ((eq? (search_state term state) (error_value)) (error_unassigned_variable term))
      ((not (eq? (search_state term state) undeclared_value)) (search_state term state))
      ; checking if undeclared
      (else (error_undeclared_variable term)))))

(trace evaluate_tree)
(trace M_state_stmt-list)
(trace M_state_stmt)
(trace call_on_stmt)
(trace declare_has_assign?)
(trace M_state_declare)
(trace M_state_assign)
(trace M_value_return)
(trace M_value_plus)
(trace M_value_minus)
(trace M_value_times)
(trace M_value_div)
(trace M_value_mod)
(trace M_value_neg)
(trace M_boolean_condition)
(trace  M_boolean_ored_expression)
(trace  M_boolean_anded_expression)
(trace compare_value)
(trace  M_boolean_compare_expression)
(trace M_value_term)