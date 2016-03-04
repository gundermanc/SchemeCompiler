; EECS 345 Programming Project, Part 1 + 2
; Case Western Reserve Univ.
;
; Christian Gunderman
; Elliot Essman
; 4 Mar. 2016

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
    (interpret_ast (state_push_scope '()) (parser filename)
                   (λ (v) (error "No return statement encountered"))
                   (λ (v) v)
                   (λ (v) (error "Continue encountered outside of loop"))
                   (λ (v) (error "Break encountered outside of loop"))
                   (λ (s v) (error "Uncaught throw")))))

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
  (λ (state ast return_state return_val continue break throw)
    (if (null? ast)
        (return_state state)
        (interpret_statement state (car ast)
                    (λ (v) (interpret_ast v (cdr ast) return_state return_val continue break throw))
                    return_val
                    continue
                    break
                    throw))))

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
  (λ (state statement return_state return_val continue break throw)
    (cond
      ((eq? 'var (operator statement)) (return_state (interpret_var state statement)))
      ((eq? '= (operator statement)) (return_state (interpret_assign state statement)))
      ((eq? 'while (operator statement)) (interpret_while state statement return_state return_val continue break throw))
      ((eq? 'return (operator statement)) (return_val (pretty_value state (operand_1 statement))))
      ((eq? 'if (operator statement)) (interpret_if state statement return_state return_val continue break throw))
      ((eq? 'begin (operator statement)) (interpret_block state statement return_state return_val continue break throw))
      ((eq? 'continue (operator statement)) (continue state))
      ((eq? 'break (operator statement)) (break state))
      ((eq? 'try (operator statement)) (interpret_try state statement return_state return_val continue break throw))
      ((eq? 'throw (operator statement)) (throw state (value state (operand_1 statement))))
      (else "invalid statement"))))

(define interpret_try
  (λ (state statement return_state return_val continue break throw)
    (interpret_ast state (cadr statement)
                   (λ (v) (interpret_finally v statement return_state return_val continue break throw))
                   return_val
                   continue
                   break
                   (λ (s v) (interpret_catch s statement
                                             (λ (v) (interpret_finally v statement return_state return_val continue break throw))
                                             return_val continue break throw v)))))

(define interpret_catch
  (λ (state statement return_state return_val continue break throw value)
    (interpret_ast (state_add (state_push_scope state) (caar (cdaddr statement)) value)
                   (cadr (cdaddr statement)) ; ast
                   (λ (v) (return_state (state_pop_scope v)))
                   return_val
                   (λ (v) (continue (state_pop_scope v)))
                   (λ (v) (break (state_pop_scope v)))
                   (λ (v) (throw (state_pop_scope v))))))

(define interpret_finally
  (λ (state statement return_state return_val continue break throw)
    (if (null? (cadddr statement))
        (return_state v)
        (interpret_ast state (cadr (cadddr statement)) return_state return_val continue break throw))))

(define interpret_block
  (λ (state statement return_state return_val continue break throw)
    (interpret_ast (state_push_scope state) (cdr statement)
                   (λ (v) (return_state (state_pop_scope v)))
                   return_val
                   (λ (v) (continue (state_pop_scope v)))
                   (λ (v) (break (state_pop_scope v)))
                   (λ (v) (throw (state_pop_scope v)))))) ;TODO: continnue, break, throw might not change scope properly.

; Interprets a var declaration statement from the AST and returns the updated state
; list.
; Throws an error if: variables are declared multiple times or have not been assigned.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; statement: a single parsed var statement.
(define interpret_var
  (λ (state statement)
    (state_update state (operand_1 statement) 0
                  (λ (v) (error "variable already declared"))
                  (λ (v) (state_add state  (operand_1 statement)
                                    (if (has_operand_2 statement)
                                                 (value state (operand_2 statement))
                                                 null))))))

; Interprets a var assign statement from the AST and returns the updated state
; list.
; Throws an error if: variable has not yet been declared.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; statement: a single parsed assign statement.
(define interpret_assign
  (λ (state statement)
    (state_update state (operand_1 statement) (value state (operand_2 statement))
                  (λ (v) v)
                  (λ (v) (error "undeclared variable in assignment")))))

; Interprets a while statement from the AST and returns the updated state
; list.
; Throws an error if: variable has not yet been declared.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; statement: a single parsed while statement.
; return: a continuation function.
(define interpret_while
  (λ (state statement return_state return_val continue break throw)
    (cond
      ((not (value state (condition statement))) (return_state state))
      (else (interpret_statement state (true_statement statement) 
                                 (λ (v) (interpret_while v statement return_state return_val continue break throw))
                                 return_val
                                 (λ (v) (interpret_while v statement return_state return_val continue break throw))
                                 (λ (v) (return_state v))
                                 throw
                                 )))))
; Interprets an if statement from the AST and returns the updated state
; list.
; Throws an error if: variable has not yet been declared or duplicate declaration.
;
; state: a list containing the current state (an empty list for first
;        execution) in the format ((K V) (K V) ..)
; statement: a single parsed if statement.
(define interpret_if
  (λ (state statement return_state return_val continue break throw)
    (if (value state (condition statement))
        (interpret_statement state (true_statement statement) return_state return_val continue break throw)
        (if (has_false_statement statement)
            (interpret_statement state (false_statement statement) return_state return_val continue break throw)
            (return_state state)))))

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

(define state_level_value
  (λ (s name return notfound)
    (cond
      ((null? s) (notfound))
      ((eq? (caar s) name) (return (cadar s)))
      (else (state_level_value (cdr s) name return notfound)))))

(define state_value
  (λ (s name)
    (if (null? s)
        (error "undefined variable" name)
        (state_level_value (car s) name
                           (λ (v) (if (null? v)
                                      (error "variable used before initialization")
                                      v))
                           (λ () (state_value (cdr s) name))))))

; Adds the specified value to the state s mapped to the specified variable
; Returns: the updated state. This does not remove existing mappings of name.
(define state_add
  (λ (s name value)
    (cons (state_level_add (car s) name value) s)))

(define state_level_add
  (λ (s name value)
    (cons (cons name (cons value '())) s)))

(define state_level_replace
  (λ (state name value replaced notreplaced)
    (cond
      ((null? state) (notreplaced '()))
      ((eq? (caar state) name) (replaced (cons (cons (caar state) (cons value '())) (cdr state))))
      (else (state_level_replace (cdr state) name value
                                 (λ (s) (replaced (cons (car state) s)))
                                 (λ (s) (notreplaced (cons (car state) s))))))))

(define state_update
  (λ (state name value updated notupdated)
    (cond
      ((null? state) (notupdated '()))
      ((null? (car state)) (state_update (cdr state) name value
                                         (λ (s2) (updated (cons (car state) s2)))
                                         (λ (s2) (notupdated (cons (car state) s2)))))
      (else (state_level_replace (car state) name value
                                 (λ (s) (updated (cons s (cdr state))))
                                 (λ (s) (state_update (cdr state) name value
                                                      (λ (s2) (updated (cons s s2)))
                                                      (λ (s2) (notupdated (cons s s2))))))))))

; Pushes a new scoping level onto the state.
; s: the state to modify.
; Returns: the updated state.
(define state_push_scope
  (λ (s)
    (cons '() s)))

; Pops a scoping level from the state.
; s: the state to modify.
; Returns: the updated state.
(define state_pop_scope cdr)
