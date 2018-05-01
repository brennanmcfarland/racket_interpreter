; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
; #lang racket
; (require "simpleParser.scm")
(load "classParser.scm")
(load "state-interpreter2-callcc-no-boxes.scm")
(load "bindings.scm")
(require racket/trace)

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
; (define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value


; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file class)
    (scheme->language
     (call/cc
      (lambda (return)
        (run-program (parser file) (newenvironment) class return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))
(trace interpret)

; perform bindings and execute main() in the interpreted program
(define run-program
  (lambda (program environment class return break continue throw)
    (eval-function (get-closure 'main (cadr (class-closure-body-methods (get-closure class (create-bindings program return break continue throw))))) '()
                   (make-new class (create-bindings program return break continue throw)) ;skipping fields going for cadr of class-closure-body-methods
                        (create-bindings program return break continue throw) return break continue throw class '())))

(trace run-program)

(define make-new
  (lambda (class environment) ;insert that object into the environment , then lookup that object
    ;(insert 'this (init-object-environment class environment) environment)))
    (init-object-environment class environment)))
; for the "outer layer" of the interpreter
; handles class bindings and the bindings for all class members before the program is evaluated
; returns the environment with bindings
(define create-bindings
  (lambda (program return break continue throw)
    (interpret-statement-list program (newenvironment) return break continue throw '() '()))) ; type and instance should never be used when creating bindings

(trace create-bindings)
; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw type instance)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw type instance) return break continue throw type instance))))

(trace interpret-statement-list)
; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw type instance)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return type instance))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment type instance))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment type instance))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw type instance))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw type instance))
      ((eq? 'continue (statement-type statement)) (continue environment type instance))
      ((eq? 'break (statement-type statement)) (break environment type instance))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw type instance))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw type instance))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw type instance))
      ((eq? 'function (statement-type statement)) (bind-function statement environment type instance))
      ((eq? 'funcall (statement-type statement)) (interpret-function
                                                  (get-closure (statement) (class-closure-body-methods (dot-get-type (function-dot statement) environment)))
                                                  (get-function-args statement)
                                                  (operand1 (function-dot statement))
                                                  (get-closure (statement) (class-closure-body-methods (dot-get-type (function-dot statement) environment)))
                                                  return break continue throw type instance))
      ((eq? 'class (statement-type statement)) (bind-class statement environment return break continue throw type instance))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; lookup the value of an object's field
(define lookup-field
  (lambda (name object environment)
    (get-index-field
     (get-field-index (class-closure-body-fields (get-closure (object-truetype (get-closure object environment)) environment)))
     (object-field-values (get-closure object environment)))))

; update the named field's value in the given object
(define update-field
  (lambda (name value object environment)
    (update-object-fields object
                          (replace-index-field
                           (get-field-index (class-closure-body-fields (get-closure (object-truetype (get-closure object environment)) environment)))
                           value
                           (object-field-values (get-closure object environment)))
                          environment))

(define get-field-index
  (lambda (name fields)
    (cond
      ((null? fields) -1)
      ((eq? name (nextof fields)) 0)
      (else (+ 1 get-field-index name (remaining fields))))))

(define get-index-field
  (lambda (index fields)
    (cond
      ((null? fields) (myerror "illegal field index" index))
      ((zero? index) (nextof fields))
      (else (get-index-field (- index 1) (remaining fields))))))

(define replace-index-field
  (lambda (index value fields)
    (cond
      ((null? fields) (myerror "illegal field index" index))
      ((zero? index) (cons value (remaining fields)))
      (else (cons (nextof fields) (replace-index-field (- index 1) (remaining fields)))))))

(define function-dot cadr)

; get the type of the dot expression
(define dot-get-type
  (lambda (statement environment)
    (get-closure (object-truetype (get-closure (operand1 statement)) (environment)))))

(trace interpret-statement)
; bind a function name to its closure
; we bind the function as if it were any other variable, with its closure as the value
(define bind-function
  (lambda (statement environment)
    (insert (get-declare-var statement) (make-function-closure statement environment) environment)))

(define bind-static-function
  (lambda (statement environment)
    (insert (get-declare-var statement) (make-function-closure statement (newenvironment)) environment)))

(define make-function-closure
  (lambda (statement environment)
    (list (operand2 statement) (operand3 statement) (lambda (newenv) (get-function-environment (get-declare-var statement) environment newenv)))))

; bind a class name to its closure
(define bind-class
  (lambda (statement environment return break continue throw type instance)
          (insert (get-declare-var statement) (make-class-closure statement environment) environment)))

(trace bind-function)
(trace bind-static-function)
(trace make-function-closure)
(trace bind-class)
; bind an object name to its closure
(define bind-object
  (lambda (statement environment type instance)
    (insert (get-declare-var statement) (make-object-closure statement environment) environment)))

(trace bind-object)
; make the closure for a class (when it's declared)
; a class closure contains these elements in order:
; 1. the super class (name)
; 2. instance field names
; 3. method bindings (names and values), including main if it exists
(define make-class-closure
  (lambda (statement environment)
    (list (superclass-name statement) (closure-fields-and-methods (class-body statement) environment '())))) ;the closure is null since it gets overridden anyway

(define object-truetype cadr)
(define object-field-values caddr)

(trace make-class-closure)
(trace object-truetype)
(trace object-field-values)
; make the closure for an object (when it's instantiated)
; an object closure contains these elements in order:
; 1. class/runtime/true type name
; 2. instance field values (TODO: including instance field values from the parent class)
(define make-object-closure
  (lambda (statement truetype environment)
    (list (object-truetype statement) (init-object-environment truetype environment))))

; update the object with a new list of field values in the environment
(define update-object-fields
  (lambda (object fields environment)
    (update object (list (object-truetype (get-closure object environment)) (fields)) environment)
    
(define closure-fields-and-methods
  (lambda (body environment closure)
    (cond
      ((null? body) (list () (newenvironment))) ; instance field names, method bindings (including main if it exists for this class)
      ((eq? 'var (statement-type (nextof body))) (add-class-closure-field (get-declare-var (nextof body)) (closure-fields-and-methods (remaining body) environment closure)))
      ((eq? 'function (statement-type (nextof body))) (add-class-closure-method (nextof body) (closure-fields-and-methods (remaining body) environment closure)))
      ((eq? 'static-function (statement-type (nextof body))) (add-class-closure-main (nextof body) (closure-fields-and-methods (remaining body) environment closure)))
      (else myerror "unable to create class closure bindings" body)
      )))

(define class-closure-body-fields car)
(define class-closure-body-methods cadr)

(trace make-object-closure)
(trace closure-fields-and-methods)
(trace class-closure-body-fields)
(trace class-closure-body-methods)
; TODO: this isn't right by any means, but what it needs to do is get the closure of the class from the program,
; then get the main method from the closure and call it
(define call-main-method
  (lambda (program class)
    (lookup 'main (class-closure-body-methods (get-closure class program)) (class-closure-body-methods (get-closure class program)))))

(define add-class-closure-field
  (lambda (name closure)
    (list (cons name (class-closure-body-fields closure)) (class-closure-body-methods closure))))

(trace call-main-method)
(trace add-class-closure-field)
; TODO: the function interpret part isn't quite right
(define add-class-closure-method
  (lambda (statement closure)
    (list (class-closure-body-fields closure) (bind-function statement (class-closure-body-methods closure)))))

(define add-class-closure-main
  (lambda (statement closure)
    (list (class-closure-body-fields closure) (bind-static-function statement (class-closure-body-methods closure)))))

(define superclass-name
  (lambda (lis)
    (if (null? (caddr lis))
        '()
        (car (cdaddr lis)))))
(define class-body cadddr)

(trace bind-function)
(trace add-class-closure-main)
(trace superclass-name)
(trace class-body)
; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return type instance)
    (return (eval-expression (get-expr statement) environment type instance))))

(trace interpret-return)
; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment type instance)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment type instance) environment)
        (insert (get-declare-var statement) 'novalue environment))))

(trace interpret-declare)
; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment type instance)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment type instance) environment)))

(trace interpret-assign)
; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw type instance)
    (cond
      ((eval-expression (get-condition statement) environment type instance) (interpret-statement (get-then statement) environment return break continue throw type instance))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw type instance))
      (else environment))))

(trace interpret-if)
; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw type instance)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment type instance)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw type instance))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

(trace interpret-while)
; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw type instance)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env))) type instance))))

(trace interpret-block)
; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw type instance)
    (throw (eval-expression (get-expr statement) environment type instance) environment)))

(trace interpret-throw)
; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block type instance)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw type instance)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2))) type instance))
                                     return break continue throw type instance)))))))

(trace create-throw-catch-continuation)
; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw type instance)
    (call/cc
     (lambda (jump)
       (trace-let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw type instance) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw type instance))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw type instance))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block type instance)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw type instance)
                          return break continue throw type instance))))))

(trace interpret-try)
; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(trace make-try-block)

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

(trace make-finally-block)
; TODO: evaluate/update state from a function call
; TODO: move interpret to the right place, if it needs to be here
; statement is the functi
(define interpret-function
  (lambda (statement args newthis environment return break continue throw type instance)
    ; TODO: run the function in the closure to get the function environment, this is the part I still don't understand
     ; it's probably related to actualize-parameters
     (interpret-statement-list (get-function-body statement)
                                ;(actualize-parameters (get-function-args statement) (get-function-params (get-closure (car (cdr (car (cadar environment)))))) (compose-closure-environment (get-closure (cdddr (car (cdr (car (cadar environment)))))) environment))
                               ; TODO: THIS IS WHERE THE ERROR IS, ACTUALIZE-PARAMETERS IS GETTING THE PARAMS WHEN IT SHOULD GET ARGS
                               ; the problem is that we're not passing it the arguments and it's instead using the params as the args
                               (insert newthis (get-closure newthis environment)
                                (actualize-parameters (eval-args args environment)
                                                      (get-function-params
                                                       statement) 
                                                      (compose-closure-environment
                                                       statement environment))) ; TODO: get rid of cdrs ;trace these functions and should just get appropriate output/input
                                ;(actualize-parameters (get-function-args statement) (get-function-params (get-closure statement)) (compose-closure-en
                                ;vironment (get-closure statement) environment))
                                ;)
                               return break continue throw type instance)))

(define eval-args
  (lambda (args environment)
    (cond
      ((null? args) '())
      ((not (isprimitive? (nextof args))) (cons (eval-expression args environment) '()))
      (else (cons (eval-expression (nextof args) environment) (eval-args (remaining args) environment))))))

; TODO: may want to actualize parameters in here instead and pass it the whole function call instead of closure and args separately to make it neater
(trace interpret-function)
; TODO: what about when the function changes global state?
(define eval-function
  (lambda (statement args this environment return break continue throw type instance)
    (interpret-function statement args this environment return break continue throw type instance)))

(trace eval-function)
; TODO: move to helper section
; given the arguments and the formal parameters, add the actual parameters to the environment
(define actualize-parameters
  (lambda (args params environment)
    (if (not (null? args))
        (insert (nextof params) (nextof args) (actualize-parameters (remaining params) (remaining args) environment))
        environment)))

(trace actualize-parameters)
; TODO: move to helper section
(define get-actual-parameters cdr)
(define nextof car)
(define remaining cdr)

; TODO: move this also
(define isprimitive?
  (lambda (expr)
    (or (number? expr) (or (eq? expr 'true) (or (eq? expr 'false))))))

(trace get-actual-parameters)
(trace nextof)
(trace remaining)
; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment type instance)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ; TODO: remove cdr
      ; TODO: may need to redefine some of the continuations here
      ((valid-function? expr environment)
       (call/cc
        (lambda (return)
          (eval-function (get-closure (cadr expr) environment) (get-function-args expr) instance environment
                                  return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown")) type instance))))
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment type instance)))))

(trace eval-expression)
; TODO: move this to helper section
; determine if the S-expression is a function in the environment
(define valid-function?
  (lambda (expr environment)
    (if (and (list? expr) (eq? (operator expr) 'funcall))
        #t
        #f
        )))
;(define valid-function?
;  (lambda (expr environment)
;    (if (and (list? expr) (exists? (get-expr expr) environment))
;        #t
;        #f)))

(trace valid-function?)
; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment type instance)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment type instance)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment type instance)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment type instance)))))

(trace eval-operator)
; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment type instance)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment type instance))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment type instance)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment type instance)))
      (else (myerror "Unknown operator:" (operator expr))))))

(trace eval-binary-op2)
; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

(trace isequal)
;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr) ;caddr
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

(trace operator)
(trace operand1)
(trace operand2)
(trace operand3)
(trace exists-operand2?)
(trace exists-operand3?)
; TODO: move to the right place
; get the function environment given the current environment
(define get-function-environment
  (lambda (name funcenvironment currentenvironment)
    (get-function-binding-values (variables-in-environment funcenvironment name) funcenvironment currentenvironment)))

; sets all values in the object's environment to a default value
(define init-object-environment
  (lambda (truetype environment)
    (set-all-values (class-closure-body-fields (get-closure truetype environment)) 'false)))

; helper function to set all values of a list to the same value
(define set-all-values
  (lambda (lis val)
    (if (null? lis)
        '()
        (cons val (set-all-values lis val)))))

;(trace get-function-environment)
; TODO: rename this and move to the appropriate place
; given the list of variables to update for the function environment, update them with the values from the current state
(define get-function-binding-values
  (lambda (vars funcenvironment currentenvironment)
    (cond
      ((null? vars) funcenvironment)
      (else (get-function-binding-values (remaining vars) (passive-update (nextof vars) (lookup (nextof vars) currentenvironment) funcenvironment) currentenvironment)))))

(trace get-function-binding-values)
; TODO: move to the right place
; get the next frame of the function environment given the current environment and add it to the function environment
;(define add-function-frame
;  (lambda (funcenvironment currentenvironment)
;    (cond
;      ((eq? (topframe funcenvironment) (newframe)) funcenvironment)
;      (())
    

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)

; TODO: move to the right place, possibly rename
(define get-closure
  (lambda (name environment)
    (lookup name environment)))
; TODO: "
(define compose-closure-environment
  (lambda (closure environment)
    ((operand2 closure) environment))) ;(list (list (operator closure) (cdadar environment))))) ;((operand2 statement) environment)))   (list (list (operator closure) (cdadar environment)))))
(define exists-declare-value? exists-operand2?)
(define get-function-body operand1)
(define get-function-args
  (lambda (statement)
    (if (list? (operand2 statement))
        (operand2 statement)
        (cons (operand2 statement) '()))))
(define get-function-params operator)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

(trace statement-type)
(trace get-expr)
(trace get-declare-var)
(trace get-declare-value)
(trace make-function-closure)
(trace get-closure)
(trace compose-closure-environment)
(trace exists-declare-value?)
(trace get-function-body)
(trace get-function-args)
(trace get-function-params)
(trace get-assign-lhs)
(trace get-assign-rhs)
(trace get-condition)
(trace get-then)
(trace get-else)
(trace get-body)
(trace exists-else?)
(trace get-try)
(trace get-catch)
(trace get-finally)
(trace catch-var)
; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

(trace language->scheme)
(trace scheme->language)
; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(trace error-break)

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

(trace myerror)