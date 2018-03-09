; Brennan McFarland
; Lucas Alva

(require racket/trace)

; given the whole state, get the list of names and values, respectively
(define state_names
  (lambda (state)
    (car state)))

(define state_values
  (lambda (state)
    (cadr state)))

;returns the value of a pair in the state ex: looking for y in (y 12) returns twleve
(define search_state
  (lambda (x state)
    (cond
      ((state_empty? state) undeclared_value)
      ((eq? x (car (state_names state))) (car (state_values state)))
      (else (search_state x (cons (cdr (state_names state)) (list (cdr (state_values state)))))))))

(trace search_state)

; perform a list operation on the state, both names and values are affected alike
(define state_listop
  (lambda (state function)
    (cons (function (state_names state)) (cons (function (state_values state)) (empty-list)))))

; helper function for if the state is empty
(define state_empty?
  (lambda (state)
    (and (null? (car state)) (null? (cadr state)))))

; adds a variable to the state with the given name and value
(define add_to_state
  (lambda (name value state)
    (cond
      ; if we didn't find a previous value, add it, and if we did, replace it
      ((state_empty? state) (cons (list name) (list (cons value (empty_list)))))
      ((feq? (state_names state) name) (replace_first_in_state name value state))
      ; if we're not at the end yet and haven't found it, recur
      (else (integrate (car (state_names state)) (car (state_values state)) (add_to_state name value (cons (cdr (car state)) (cons (cdr (car (cdr state))) '()))))))))

; replaces the first in state
(define replace_first_in_state
  (lambda (name value state)
    (cons (cons name (cdr (state_names state)))(list(cons value (cdr (state_values state)))))))

(trace add_to_state)

; check if a variable is in the state
(define state_contains?
  (lambda (name state)
    (cond
      ; we didn't find a previous value
      ((state_empty? state) #f)
      ((feq? (state_names state) name) #t)
      ; if we're not at the end yet and haven't found it, recur
      (else (state_contains? name (cons (cdr (state_names state)) (cons (cdr (state_values state)) (empty_list))))))))

; (trace state_contains?)

; put the new name and value into the updated state
(define integrate
  (lambda (name value state)
    (cons (cons name (state_names state)) (cons (cons value (state_values state)) (empty_list)))))
