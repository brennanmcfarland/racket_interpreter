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

; a helper function: given a nonterminal, determine if it is a <type>
(define type?
  (lambda (nonterminal)
    (eq? nonterminal 'var))) ;TODO: this is a slight shortcut from the BNF, should probably change
;one or the other

; racket supports short circuit evaluation, so we can write this as one conditional
(define M_state_stmt
  (lambda (stmt state)
    (cond
      ((feq? stmt 'if) (M_state_if stmt state))
      ((feq? stmt 'while) (M_state_while stmt state))
      ((type? (car stmt)) (M_state_declare stmt state))
      ((feq? stmt '=) (M_state_assign stmt state))
      ((feq? stmt 'return) (M_state_return stmt state)) ;TODO: forgot about this in the BNF!
      (else error))))

(define M_state_if
  (lambda (stmt state)
    state ;TODO
    ))

(define M_state_while
  (lambda (stmt state)
    state ;TODO
    ))

(define M_state_declare
  (lambda (stmt state)
    state ;TODO
    ))

(define M_state_assign
  (lambda (stmt state)
    state ;TODO
    ))

(define M_state_return
  (lambda (stmt state)
    state ;TODO
    ))