; Brennan McFarland
; Lucas Alva

(require racket/trace)
(load "Definitions.rkt") ; TODO: remove to avoid cyclic dependencies

; given the current stack frame, get the list of in-frame names and values, respectively
(define frame_names
  (lambda (frame)
    (car frame)))

(define frame_values
  (lambda (frame)
    (cadr frame)))

; get the state's current/other stack frames
(define current_frame
  (lambda (state)
    (car state)))

(define other_frames
  (lambda (state)
    (cdr state)))

; given the current stack frame, get the next/remaining name and value, respectively
(define next_frame_name
  (lambda (frame)
    (car (frame_names frame))))

(define next_frame_value
  (lambda (frame)
    (car (frame_values frame))))

(define remaining_frame_names
  (lambda (frame)
    (cdr (frame_names frame))))

(define remaining_frame_values
  (lambda (frame)
    (cdr (frame_values frame))))

; returns the value of a pair in the current stack frame
(define search_state
  (lambda (x state)
    (cond
      ((state_empty? state) (undeclared_value))
      ((frame_contains? x (current_frame state)) (search_frame x (current_frame state)))
      (else (search_state x (other_frames state))))))

(define search_frame
  (lambda (x frame)
    (cond
      ((frame_empty? frame) (undeclared_value))
      ((eq? x (next_frame_name frame)) (next_frame_value frame))
      (else (search_frame x (cons (remaining_frame_names frame) (list (remaining_frame_values frame))))))))

; perform a list operation on the in-frame state, both names and values are affected alike
(define frame_listop
  (lambda (frame function)
    (cons (function (frame_names frame)) (cons (function (frame_values frame)) (empty-list)))))

; helper function for if the state is empty
(define state_empty?
  (lambda (state)
    (or (null? state) (and (null? (other_frames state)) (frame_empty? (current_frame state)))))) ; TODO: remove this, we will push a stack frame every time instead
  
; helper function for if the in-frame state is empty
(define frame_empty?
  (lambda (frame)
    (and (null? (frame_names frame)) (null? (frame_values frame)))))

; adds a variable to the in-frame state with the given name and value
(define add_to_state
  (lambda (name value state)
    (cons (add_to_frame name value (current_frame state)) (other_frames state))))

(define add_to_frame
  (lambda (name value frame)
    (cond
      ; if we didn't find a previous value, add it, and if we did, replace it
      ((frame_empty? frame) (cons (list name) (list (cons value (empty_list)))))
      ((feq? (frame_names frame) name) (replace_first_in_frame name value frame))
      ; if we're not at the end yet and haven't found it, recur
      (else (integrate_in_frame (next_frame_name frame) (next_frame_value frame) (add_to_frame name value (cons (remaining_frame_names frame) (cons (remaining_frame_values frame) '()))))))))

; replaces the first in frame
(define replace_first_in_frame
  (lambda (name value frame)
    (cons (cons name (remaining_frame_names frame))(list(cons value (remaining_frame_values frame))))))

; check if a variable is in the state (from any frame)
(define state_contains?
  (lambda (name state)
    (cond
      ((state_empty? state) #f)
      ((frame_contains? name (current_frame state)) #t)
      (else (state_contains? name (other_frames state))))))

; check if a variable is in the frame
(define frame_contains?
  (lambda (name frame)
    (cond
      ; we didn't find a previous value
      ((frame_empty? frame) #f)
      ((feq? (frame_names frame) name) #t)
      ; if we're not at the end yet and haven't found it, recur
      (else (frame_contains? name (cons (remaining_frame_names frame) (cons (remaining_frame_values frame) (empty_list))))))))

; put the new name and value into the updated frame frame
(define integrate_in_frame
  (lambda (name value frame)
    (cons (cons name (frame_names frame)) (cons (cons value (frame_values frame)) (empty_list)))))

;(trace search_frame)
;(trace add_to_frame)
;(trace frame_contains?)