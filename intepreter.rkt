(load "simpleParser.scm")

;Start interpreting a file
(define start
  (lambda (filename)
    (interpret_all '() (parser filename))))

;Interpret a list of commands
(define interpret_all
  (lambda (state commands)
    (cond
      ((null? commands) state)
      ((eq? 'return (caar commands)) (value state (cadar commands)))
      (else 
        (interpret_all (interpret state (car commands)) (cdr commands))))))

;Interpret a single command and return the new state
(define interpret
  (lambda (state command)
    (cond
      ((eq? 'var (operator command)) (interpret_var state command))
      ((eq? '= (operator command)) (interpret_assign state command))
      ((eq? 'while (operator command)) (interpret_while state command))
      ((eq? 'if (operator command)) (interpret_if state command)))))

; Declares a variable, throws an error if it already exists.
(define interpret_var
  (lambda (state command)
    (cond
      ((state_exists state (operand_1 command)) (error "variable already declared"))
      ((has_operand_2 command) (state_update state (operand_1 command) (value state (operand_2 command))))
      (else (state_update state (operand_1 command) 0)))))

; Assigns a variable, throws an error if it already exists.
(define interpret_assign
  (lambda (state command)
    (cond
      ((state_update state (operand_1 command) (value state (operand_2 command)))))))

(define interpret_while
  (lambda (state command)
    (cond
      ((eq? (value state (condition command)) #f) state)
      (else (interpret_while (interpret state (true_statement command)) command)))))

; Interprets an in statement and runs either the true statement or false statement.
(define interpret_if
  (lambda (state command)
    (if (value state (condition command))
        (interpret state (true_statement command))
        (if (has_false_statement command)
            (interpret state (false_statement command))))))

(define condition cadr)
(define true_statement caddr)
(define false_statement cadddr)
(define has_false_statement
  (lambda (expression)
      (not (null? (cdddr expression)))))

(define operator
  (lambda (expression)
    (car expression)))

(define operand_1
  (lambda (expression)
    (cadr expression)))

(define operand_2
  (lambda (expression)
    (caddr expression)))

(define has_operand_2
  (lambda (expression)
    (not (null? (cddr expression)))))

(define operand_3
  (lambda (expression)
    (cadddr expression)))

(define has_operand_3
  (lambda (expression)
    (not (null? (cdddr expression)))))

(define state_exists
  (lambda (s name)
    (cond
      ((null? s) #f)
      ((eq? (caar s) name) #t)
      (else (state_exists (cdr s) name)))))

(define state_value
  (lambda (s name)
    (cond
      ((null? s) (error "undefined variable"))
      ((eq? (caar s) name) (cadar s))
      (else (state_exists (cdr s) name)))))

; Adds the specified value to the state s mapped to the specified variable
; Returns the updated state. This does not remove existing mappings of name.
(define state_add
  (lambda (s name value)
    (cons (cons name (cons value '())) s)))

; Removes all instances of the specified value from the state s if it exists.
; Returns the updated state.
(define state_remove
  (lambda (s name)
    (cond
      ((null? s) '())
      ((eq? (caar s) name) (cdr s))
      (else (cons (car s) (state_remove (cdr s) name))))))

; Adds or updates a mapping from name to value in state s
; and returns the updated state.
(define state_update
  (lambda (s name value)
    (state_add (state_remove s name) name value)))

;; Looks up an arithmetic function by its symbol.
(define operation_function
  (lambda (operator)
    (cond
      ((eq? '+ operator) (lambda (a b) (+ a b)))
      ((eq? '- operator) (lambda (a b) (- a b)))
      ((eq? '* operator) (lambda (a b) (* a b)))
      ((eq? '/ operator) (lambda (a b) (quotient a b)))
      ((eq? '% operator) (lambda (a b) (remainder a b)))
      ((eq? '< operator) (lambda (a b) (< a b)))
      ((eq? '> operator) (lambda (a b) (> a b)))
      ((eq? '<= operator) (lambda (a b) (<= a b)))
      ((eq? '>= operator) (lambda (a b) (>= a b)))
      ((eq? '== operator) (lambda (a b) (eq? a b)))
      ((eq? '!= operator) (lambda (a b) (not (eq? a b))))
      ((eq? '&& operator) (lambda (a b) (and a b)))
      ((eq? '|| operator) (lambda (a b) (or a b)))
      ((eq? '! operator) (lambda (a b) (not b)))
      )))


;; Gets the value of an expression.
(define value
  (lambda (s expression)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (list? expression)) (state_value s expression))
      ((not (has_operand_2 expression))((operation_function (operator expression))
             0
             (value s (operand_1 expression))))
      (else ((operation_function (operator expression))
             (value s (operand_1 expression))
             (value s (operand_2 expression)))))))
