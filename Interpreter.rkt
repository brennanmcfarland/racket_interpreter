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
        (M_state_stmt_list (cdr stmt-list) (M_state_stmt (car stmt-list) state)))))

(define M_state_stmt
  (lambda (stmt state)
    ; TODO
    ))