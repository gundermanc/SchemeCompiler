; EECS 345 Programming Project, Part 1 + 2 + 3
; Case Western Reserve Univ.
;
; Christian Gunderman
; Elliot Essman
; 29 Mar. 2016

(load "functionParser.scm")

(define interpret_new
  (λ (filename)
    ((lambda (return_cont continue_cont break_cont throw_cont)
       (interpret_ast_new (env_push '()) (parser filename)
                          interpret_toplevel_statement
                          (λ (env) (call_function
                                        env
                                        'main
                                        '()
                                        (λ (v) (error "No return encountered in main"))
                                        return_cont
                                        continue_cont
                                        break_cont
                                        throw_cont))
                          return_cont     ; TODO: this shouldn't be possible.
                          continue_cont   ; TODO: this shouldn't be possible.
                          break_cont      ; TODO: this shouldn't be possible.
                          throw_cont))
     (λ (v) (return_value v))
     (λ (v) (error "Continue encountered outside of loop"))
     (λ (v) (error "Break encountered outside of loop"))
     (λ (s v) (error "Uncaught throw")))))
                         

(define interpret_ast_new
  (λ (env ast stmt_interpreter env_cont return_cont continue_cont break_cont throw_cont)
    (if (null? ast)
        (env_cont env)
        (stmt_interpreter env
                          (current_statement ast)
                          (λ (v) (interpret_ast_new v (remaining_statements ast) stmt_interpreter env_cont return_cont continue_cont break_cont throw_cont))
                          return_cont
                          continue_cont
                          break_cont
                          throw_cont))))

(define interpret_toplevel_statement
  (λ (env statement env_cont return_cont continue_cont break_cont throw_cont)
    (cond
      ((eq? 'var (operator statement)) (interpret_var env statement env_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'function (operator statement)) (interpret_function env statement env_cont return_cont continue_cont break_cont throw_cont))
      (else (error "invalid top level statement" (operator statement))))))

(define interpret_body_statement
  (λ (env statement env_cont return_cont continue_cont break_cont throw_cont)
    (cond
      ((eq? 'var (operator statement)) (interpret_var env statement env_cont return_cont continue_cont break_cont throw_cont)) ; TODO test.
      ((eq? 'function (operator statement)) (interpret_function env statement env_cont return_cont continue_cont break_cont throw_cont)) ; TODO test
      ((eq? 'return (operator statement)) (return_cont (build_return env (value env (operand_1 statement)))))
      ((eq? '= (operator statement)) (env_cont (interpret_assign env statement)))
      ((eq? 'while (operator statement)) (interpret_while env statement env_cont return_cont continue_cont break_cont throw_cont))
      (else (error "invalid body statement" (operator statement))))))

(define interpret_function
  (λ (env statement env_cont return_cont continue_cont break_cont throw_cont)
    (env_cont (env_current_func_add env (cdr statement)))))

(define call_function
  (λ (env name args env_cont return_cont continue_cont break_cont throw_cont)
    (interpret_ast_new env
                       (caddr (env_current_func_lookup env name))
                       interpret_body_statement
                       env_cont
                       return_cont
                       continue_cont
                       break_cont
                       throw_cont)))
               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OLD INTERPRETER ;;;;;;;;;;;;;;;;;;;;;;;;;

; External dependencies
; ==========================================================
;(load "simpleParser.scm")

; Public "API"
; ==========================================================

; Interprets a file and returns a single value containing result
; of the program execution (whatever was returned using the return
; statement that was executed.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block. 
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

; "Private" Impl:
; ==========================================================

; Interprets an AST.
;
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
;
; state: the current program state.
; ast: a properly formed abstract syntax tree of the format output
;      by simpleParser.scm
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_ast
  (λ (state ast state_cont return_cont continue_cont break_cont throw_cont)
    (if (null? ast)
        (state_cont state)
        (interpret_statement state (current_statement ast)
                             (λ (v) (interpret_ast v (remaining_statements ast) state_cont return_cont continue_cont break_cont throw_cont))
                             return_cont
                             continue_cont
                             break_cont
                             throw_cont))))

; Interprets a single statement from the AST and updates the state.
; 
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
;
;
; state: the current program state.
; statement: a properly formed abstract syntax tree statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_statement
  (λ (state statement state_cont return_cont continue_cont break_cont throw_cont)
    (cond
      ((eq? 'var (operator statement)) (state_cont (interpret_var state statement)))
      ((eq? '= (operator statement)) (state_cont (interpret_assign state statement)))
      ((eq? 'while (operator statement)) (interpret_while state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'return (operator statement)) (return_cont (pretty_value state (operand_1 statement))))
      ((eq? 'if (operator statement)) (interpret_if state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'begin (operator statement)) (interpret_block state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'continue (operator statement)) (continue_cont state))
      ((eq? 'break (operator statement)) (break_cont state))
      ((eq? 'try (operator statement)) (interpret_try state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'throw (operator statement)) (throw_cont state (value state (operand_1 statement))))
      (else "invalid statement"))))

; Interprets a try/catch/finally block/statement.
;
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
;
;
; state: the current program state.
; statement: a properly formed abstract syntax tree try/catch/finally statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_try
  (λ (state statement state_cont return_cont continue_cont break_cont throw_cont)
    (interpret_ast state (cadr statement)
                   (λ (v) (interpret_finally v statement state_cont return_cont continue_cont break_cont throw_cont))
                   return_cont
                   continue_cont
                   break_cont
                   (λ (s v) (interpret_catch s statement
                                             (λ (v) (interpret_finally v statement
                                                                       state_cont
                                                                       return_cont
                                                                       continue_cont
                                                                       break_cont
                                                                       throw_cont))
                                             return_cont continue_cont break_cont throw_cont v)))))

; Interprets a catch block/statement.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
;
;
; state: the current program state.
; statement: a properly formed abstract syntax tree try/catch/finally statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_catch
  (λ (state statement state_cont return_cont continue_cont break_cont throw_cont value)
    (interpret_ast (state_add (state_push_scope state) (catch_var statement) value)
                   (try_block statement)
                   (λ (v) (state_cont (state_pop_scope v)))
                   return_cont
                   (λ (v) (continue_cont (state_pop_scope v)))
                   (λ (v) (break_cont (state_pop_scope v)))
                   (λ (s v) (throw_cont (state_pop_scope s) v)))))

; Interprets a finally block/statement.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
;
;
; state: the current program state.
; statement: a properly formed abstract syntax tree try/catch/finally statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_finally
  (λ (state statement state_cont return_cont continue_cont break_cont throw_cont)
    (if (null? (finally_stmt statement))
        (state_cont state)
        (interpret_ast state (finally_block statement) state_cont return_cont continue_cont break_cont throw_cont))))

; Interprets a block statement.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
;
;
; state: the current program state.
; statement: a properly formed abstract syntax tree block statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_block
  (λ (state statement state_cont return_cont continue_cont break_cont throw_cont)
    (interpret_ast (state_push_scope state) (cdr statement)
                   (λ (v) (state_cont (state_pop_scope v)))
                   return_cont
                   (λ (v) (continue_cont (state_pop_scope v)))
                   (λ (v) (break_cont (state_pop_scope v)))
                   (λ (s v) (throw_cont (state_pop_scope s) v)))))

(define interpret_var
  (λ (env statement env_cont return_cont continue_cont break_cont throw_cont)
    (env_cont
     (env_current_value_update env
                               (operand_1 statement) 0
                               (λ (v) (error "variable already declared:" (operand_1 statement)))
                               (λ (v) (env_current_value_add env  (operand_1 statement)
                                                             (if (has_operand_2 statement)
                                                                 (value (env_current_state env) (operand_2 statement))
                                                                 null)))))))

; Interprets a var assign statement from the AST and returns the updated state.
; Throws an error if: variable has not yet been declared.
;
; state: a list containing the current state.
; statement: a single parsed assign statement.
(define interpret_assign
  (λ (env statement)
    (env_current_value_update env (operand_1 statement) (value env (operand_2 statement))
                  (λ (v) v)
                  (λ (v) (error "undeclared variable in assignment")))))

; Interprets a while loop.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
;
; state: the current program state.
; statement: a properly formed abstract syntax tree while loop.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_while
  (λ (env statement env_cont return_cont continue_cont break_cont throw_cont)
    (cond
      ((not (value env (condition statement))) (env_cont env))
      (else (interpret_body_statement env (true_statement statement)
                                 (λ (v) (interpret_while v statement env_cont return_cont continue_cont break_cont throw_cont))
                                 return_cont
                                 (λ (v) (interpret_while v statement env_cont return_cont continue_cont break_cont throw_cont))
                                 (λ (v) (env_cont v))
                                 throw_cont)))))

; Interprets an if statement.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
;
;
; state: the current program state.
; statement: a properly formed abstract syntax tree if statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_if
  (λ (state statement state_cont return_cont continue_cont break_cont throw_cont)
    (if (value state (condition statement))
        (interpret_statement state (true_statement statement) state_cont return_cont continue_cont break_cont throw_cont)
        (if (has_false_statement statement)
            (interpret_statement state (false_statement statement) state_cont return_cont continue_cont break_cont throw_cont)
            (state_cont state)))))

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
; env: current environment stack (list).
; expression: the expression AST.
; Throws error if: variable is used before it is declared or an unknown
; operation is attempted.
; Returns: the value of the expression.
(define value
  (λ (env expression)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (list? expression)) (env_current_value env expression))
      ((not (has_operand_2 expression))((operation_function (operator expression))
             0
             (value env (operand_1 expression))))
      (else ((operation_function (operator expression))
             (value env (operand_1 expression))
             (value env (operand_2 expression)))))))

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

(define build_return
  (λ (env value)
    (cons env (cons value '()))))

(define return_env car)
(define return_value cadr)

; Gets the finally portion of the try/catch block/statement.
(define finally_stmt cadddr)

; Gets the finally code block from the try/catch statement.
(define finally_block
  (λ (statement)
    (cadr (finally_stmt statement))))

; Gets the catch var from the try/catch block/statement.
(define catch_var
  (λ (statement)
    (caar (cdaddr statement))))

; Gets the try code block from the try/catch statement.
(define try_block
  (λ (statement)
    (cadr (cdaddr statement))))

; Gets the current statement from an AST.
(define current_statement car)

; Gets the remaining statements after the current from an AST.
(define remaining_statements cdr)

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


;;;;;;;;;;;;;;; TBD DELETE THIS STUFF ;;;;;;;;;;;;;;;;;;;;;;


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









;;;;;;;;;;;;;;; FINALIZED UPDATED FROM HERE ON ;;;;;;;;;;;;;;;;;;;;;;


; Builds a new, empty environment list for containing a list of functions
;
; Format: ((func_pairs) (state_list) Example: ((foo (code)) (((x 3) (y 2)) ((z 1)))
; functions: a list of key value pairs containing functions and code.
; state: a list of scopes containing variable definitions.
; Throws: No error checking. Be very careful that inputs are the correct form.
(define env_build
  (λ (functions state)
    (cons functions (cons state '()))))

; Pushes a new environment to the environment stack (list). Env should be constructed
; with env_build or similar.
;
; env: See env_build for format info.
; Throws: No error checking. Be very careful that inputs are the correct form.
(define env_push
  (λ (env)
     (cons (env_build '() '(())) env)))

; Updates the current environment with new functions and/or state.
; Throws: No error checking. Be very careful that inputs are the correct form.
(define env_current_update
  (λ (env functions state)
    (cons (env_build functions state) (env_pop env))))

; Pops an environment from the environment stack (list).
; Throws: No error checking. Be very careful that inputs are the correct form.
(define env_pop cdr)

; Returns the list of functions from the current environment.
;
; env: the stack (list) of environments.
; Format: ((func1_name (code)) (func2_name (code)) ...)
; Throws: No error checking. Be very careful that inputs are the correct form.
(define env_current_funcs caar)

; Returns the state for the current environment.
; env: the stack (list) of environments.
; Format: ((nscope) (n-1scope) (n-2scope) ...)
; Throws: No error checking. Be very careful that inputs are the correct form.
(define env_current_state cadar)

; Pushes a new scope into the current environment on the env stack (list).
; env: The environment stack (list).
; Format: See env_build for more info.
; Throws: No error checking. Be very careful that inputs are the correct form.
(define env_scope_push
  (λ (env)
    (env_current_update env (env_current_funcs env) (state_scope_push (env_current_state env)))))

; Pops new scope from the current environment on the env stack (list).
; env: The environment stack (list).
; Format: See env_build for more info.
; Throws: No error checking. Be very careful that inputs are the correct form.
(define env_scope_pop
  (λ (env)
    (env_current_update env (env_current_funcs env) (state_scope_pop (env_current_state env)))))

; Pushes a new scoping level onto the state.
; s: the state to modify.
; Returns: the updated state.
(define state_scope_push
  (λ (s)
    (cons '() s)))

; Pops a scoping level from the state.
; s: the state to modify.
; Returns: the updated state.
(define state_scope_pop cdr)

; Gets the value of a variable in a specific level of the state.
; s: state scope level list.
; name: the name of the variable.
; return_cont: the return continuation for the value.
; nonfound_cont: the continatuion for values that were not able to be found.
(define state_level_value
  (λ (s name return_cont notfound_cont)
    (cond
      ((null? s) (notfound_cont))
      ((eq? (caar s) name) (return_cont (cadar s)))
      (else (state_level_value (cdr s) name return_cont notfound_cont)))))

; Iterates the stack of scopes in the state, starting from current and going up
; to find the value of the given var.
; s: the state
; name: the name of the variable.
; Returns the variable.
; Throws: undefined var if it doesn't exist or used before initialization if the
; var was declared without a initial value.
(define state_value
  (λ (s name)
    (if (null? s)
        (error "undefined variable" name)
        (state_level_value (car s) name
                           (λ (v) (if (null? v)
                                      (error "variable used before initialization")
                                      v))
                           (λ () (state_value (cdr s) name))))))

; Adds the specified value to the current environment mapped to the specified variable
; in the current scope.
; Returns: the updated state. This does not remove existing mappings of name.
(define env_current_value
  (λ (env name)
    (state_value (env_current_state env) name)))


; Adds the specified value to the state s mapped to the specified variable
; in the current scope.
; Returns: the updated state. This does not remove existing mappings of name.
(define state_add
  (λ (s name value)
    (cons (state_level_add (car s) name value) (cdr s))))

; Adds the specified value to the current environment mapped to the specified variable
; in the current scope.
; Returns: the updated state. This does not remove existing mappings of name.
(define env_current_value_add
  (λ (env name value)
    (env_current_update env (env_current_funcs env) (state_add (env_current_state env) name value))))


; Adds the specified value to the given level of the state.
; s: a state level.
; name: the name of the variable.
; value: the value to map to the name.
(define state_level_add
  (λ (s name value)
    (cons (cons name (cons value '())) s)))

; Iterates through the current level of the scope, heading out
; until it locates the existing mapping for the variable and replaces it.
; state: the state.
; name: the name of the variable.
; value: the new value.
; replaced_cont: the continuation for if the value was replaced.
; notreplaced_cont: the continuation for if the value was not replaced.
(define state_level_replace
  (λ (state name value replaced_cont notreplaced_cont)
    (cond
      ((null? state) (notreplaced_cont '()))
      ((eq? (caar state) name) (replaced_cont (cons (cons (caar state) (cons value '())) (cdr state))))
      (else (state_level_replace (cdr state) name value
                                 (λ (s) (replaced_cont (cons (car state) s)))
                                 (λ (s) (notreplaced_cont (cons (car state) s))))))))

; Adds a function to the functions list. Although this uses state_level_replace it
; has nothing to do with the state.
(define env_current_func_add
  (λ (env func)
    (env_current_update env
                        (cons func
                              (map (λ (v)
                                     (if (eq? (car v) (car func))
                                         (error "Duplicate function" (car func))
                                         v))
                                   (env_current_funcs env)))
                        (env_current_state env))))

; Finds a function in the functions list.
(define func_lookup
  (λ (funcs name)
    (cond
      ((null? funcs) (error "Unknown function name" name))
      ((eq? name (caar funcs)) (car funcs))
      (else (func_lookup (cdr funcs) name)))))

; Finds a function in the functions list.
(define env_current_func_lookup
  (λ (env name)
    (func_lookup (env_current_funcs env) name)))
    
; Iterates through the levels of the scope, starting from current, heading out
; until it locates the existing mapping for the variable and replaces it.
; state: the state.
; name: the name of the variable.
; value: the new value.
; updated_cont: the continuation for if the value was replaced.
; notupdated_cont: the continuation for if the value was not replaced.
(define state_update
  (λ (state name value updated_cont notupdated_cont)
    (cond
      ((null? state) (notupdated_cont '()))
      ((null? (car state)) (state_update (cdr state) name value
                                         (λ (s2) (updated_cont (cons (car state) s2)))
                                         (λ (s2) (notupdated_cont (cons (car state) s2)))))
      (else (state_level_replace (car state) name value
                                 (λ (s) (updated_cont (cons s (cdr state))))
                                 (λ (s) (state_update (cdr state) name value
                                                      (λ (s2) (updated_cont (cons s s2)))
                                                      (λ (s2) (notupdated_cont (cons s s2))))))))))

; Iterates through the levels of the scope, starting from current, heading out
; until it locates the existing mapping for the variable and replaces it.
; env: the environment stack (list).
; name: the name of the variable.
; value: the new value.
; updated_cont: the continuation for if the value was replaced.
; notupdated_cont: the continuation for if the value was not replaced.
(define env_current_value_update
  (λ (env name value updated_cont notupdated_cont)
    (state_update (env_current_state env) name value 
                  (λ (v) (env_current_update env (env_current_funcs env) v))
                  notupdated_cont)))