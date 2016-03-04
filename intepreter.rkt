; EECS 345 Programming Project, Part 1
; Case Western Reserve Univ.
;
; Christian Gunderman
; Elliot Essman
; 2 Feb. 2016

; External dependencies
; ==========================================================
(load "simpleParser.scm")

; Public "API"
; ==========================================================

; Interprets a file and returns a single value containing result
; of the program execution (whatever was returned using the return
; statement that was executed.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times.
;
; filename: the name of the input file.
(define interpret
  (λ (filename)
    (interpret_ast '() (parser filename) (λ (v) v))))

; Interprets an AST and returns a single value containing result
; of the program execution (whatever was returned using the return
; statement that was executed. Assumes correct AST format.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times.
;
; ast: a properly formed abstract syntax tree of the format output
;      by simpleParser.scm
(define interpret_ast
  (λ (state ast return)
    (if (null? ast)
        (return state) ;; TODO: check if we saw a return.
        (interpret_statement state (car ast) (λ (v) (return (interpret_ast v (cdr ast) return)))))))

; "Private" Impl:
; ==========================================================

; Interprets a single statement from the AST and returns the updated state
; list containing values for variables.
; Throws an error if: variables are used before being declared or
; variables are declared multiple times.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; ast: the abstract syntax tree.
; return: return continuation function.
(define interpret_statement
  (λ (state statement return)
    (cond
      ((eq? 'var (operator statement)) (return (interpret_var state statement)))
      ((eq? '= (operator statement)) (return (interpret_assign state statement)))
      ((eq? 'while (operator statement)) (interpret_while state statement return))
      ((eq? 'return (operator statement)) (return (pretty_value state (operand_1 statement))))
      ((eq? 'if (operator statement)) (interpret_if state statement return))
      (else "invalid statement"))))

; Interprets a var declaration statement from the AST and returns the updated state
; list.
; Throws an error if: variables are declared multiple times or have not been assigned.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; statement: a single parsed var statement.
(define interpret_var
  (λ (state statement)
    (cond
      ((state_exists state (operand_1 statement)) (error "variable already declared"))
      ((has_operand_2 statement) (state_update state (operand_1 statement) (value state (operand_2 statement))))
      (else (state_update state (operand_1 statement) null)))))

; Interprets a var assign statement from the AST and returns the updated state
; list.
; Throws an error if: variable has not yet been declared.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; statement: a single parsed assign statement.
(define interpret_assign
  (λ (state statement)
    (if (state_exists state (operand_1 statement))
        (state_update state (operand_1 statement) (value state (operand_2 statement)))
        (error "Undeclared variable in assignment"))))

; Interprets a while statement from the AST and returns the updated state
; list.
; Throws an error if: variable has not yet been declared.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; statement: a single parsed while statement.
; return: a continuation function.
(define interpret_while
  (λ (state statement return)
    (cond
      ((not (value state (condition statement))) (return state))
      (else (interpret_statement state (true_statement statement) (λ (v) (interpret_while v statement return)))))))

; Interprets an if statement from the AST and returns the updated state
; list.
; Throws an error if: variable has not yet been declared or duplicate declaration.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; statement: a single parsed if statement.
(define interpret_if
  (λ (state statement return)
    (if (value state (condition statement))
        (interpret_statement state (true_statement statement) return)
        (if (has_false_statement statement)
            (interpret_statement state (false_statement statement) return)
            (return state)))))

; Looks up an arithmetic or boolean function by its symbol.
; operator: an arithmetic or boolean operator.
; Throws error if: unknown operator.
; Returns: an operator function that requires two operands.
;          If operator is also a unary operator, set a to zero
;          and b to the unary operand.
(define operation_function
  (λ (operator)
    (cond
      ((eq? '+ operator) (λ (a b) (+ a b)))
      ((eq? '- operator) (λ (a b) (- a b)))
      ((eq? '* operator) (λ (a b) (* a b)))
      ((eq? '/ operator) (λ (a b) (quotient a b)))
      ((eq? '% operator) (λ (a b) (remainder a b)))
      ((eq? '< operator) (λ (a b) (< a b)))
      ((eq? '> operator) (λ (a b) (> a b)))
      ((eq? '<= operator) (λ (a b) (<= a b)))
      ((eq? '>= operator) (λ (a b) (>= a b)))
      ((eq? '== operator) (λ (a b) (eq? a b)))
      ((eq? '!= operator) (λ (a b) (not (eq? a b))))
      ((eq? '&& operator) (λ (a b) (and a b)))
      ((eq? '|| operator) (λ (a b) (or a b)))
      ((eq? '! operator) (λ (a b) (not b)))
      (else (error "undefined operator" operator))
      )))


; Evaluates the value of an expression, including constants,
; arithmetic operations, boolean operations, and variable references.
; s: current state list.
; expression: the expression AST.
; Throws error if: variable is used before it is declared or an unknown
; operation is attempted.
; Returns: the value of the expression.
(define value
  (λ (s expression)
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

; Wraps the value function such as to return non-lisp versions
; of boolean values.
(define pretty_value
  (λ (s expression)
    ((λ (retval)
      (cond
        ((eq? retval #t) 'true)
        ((eq? retval #f) 'false)
        (else retval)))
     (value s expression))))

; Helper functions:
; ==========================================================

; Gets the condition of an IF or WHILE.
(define condition cadr)

; Gets the statement executed if true of an IF or WHILE.
(define true_statement caddr)

; Get the statement executed if false of an IF (else statement).
(define false_statement cadddr)

; Checks if an IF has an else statement.
(define has_false_statement
  (λ (expression)
      (not (null? (cdddr expression)))))

; Gets an operator from an operation.
(define operator car)

; Gets the left operand from an operation.
(define operand_1 cadr)

; Gets the right operand from an operation.
(define operand_2 caddr)

; Checks if an operation has two or more operands.
(define has_operand_2
  (λ (expression)
    (not (null? (cddr expression)))))

; Checks if the given name exists in the state list.
; Returns: true if the name exists, false if it does not.
(define state_exists
  (λ (s name)
    (cond
      ((null? s) #f)
      ((eq? (caar s) name) #t)
      (else (state_exists (cdr s) name)))))

; Gets the value of the given name from the state list.
; Returns: the value. Behavior on name not mapped is undefined.
(define state_value
  (λ (s name)
    (cond
      ((null? s) (error "undefined variable"))
      ((eq? (caar s) name) (if (null? (cadar s))
                               (error "uninitialized variable")
                               (cadar s)))
      (else (state_value (cdr s) name)))))

; Adds the specified value to the state s mapped to the specified variable
; Returns: the updated state. This does not remove existing mappings of name.
(define state_add
  (λ (s name value)
    (cons (cons name (cons value '())) s)))

; Removes all instances of the specified value from the state s if it exists.
; Returns the updated state.
(define state_remove
  (λ (s name)
    (cond
      ((null? s) '())
      ((eq? (caar s) name) (cdr s))
      (else (cons (car s) (state_remove (cdr s) name))))))

; Adds or updates a mapping from name to value in state s
; and returns the updated state.
; This is the preferred way to mapping a name to a state value.
(define state_update
  (λ (s name value)
    (state_add (state_remove s name) name value)))

