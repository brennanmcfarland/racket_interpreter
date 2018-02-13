(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (evaluate_tree (parser filename))
    ; TODO: call parser with filename, evaluate parse tree,
    ; and return the right value
    ))

(define evaluate_tree
  (lambda tree
    tree
    ; TODO: instead of just returning the tree, evaluate the
    ; parse tree and return the proper value
    ))