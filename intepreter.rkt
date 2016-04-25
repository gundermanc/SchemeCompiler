; EECS 345 Programming Project, Part 1 + 2 + 3
; Case Western Reserve Univ.
;
; Christian Gunderman
; Elliot Essman
; 29 Mar. 2016

; External dependencies
; ==========================================================
(load "classParser.scm")

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
    ((λ (state_cont return_cont continue_cont break_cont throw_cont)
      (interpret_ast (state_push_scope (state_empty)) (parser filename)
                     (λ (state) (call_function state 'main '() state_cont return_cont
                                               continue_cont
                                               break_cont
                                               throw_cont))
                     return_cont
                     continue_cont
                     break_cont
                     throw_cont))
     (λ (v) v); (error "No return statement encountered"))
     (λ (v) (pretty_value v))
     (λ (v) (error "Continue encountered outside of loop"))
     (λ (v) (error "Break encountered outside of loop"))
     (λ (v) (error "Uncaught throw")))))

; "Private" Impl:
; ==========================================================

; Accepts an AST and parses the classes and launches the main
; method of the designated class.
; ast: the abstract syntax tree.
; class_name: the symbol of the desired main class.
(define interpret_toplevel_ast
  (λ (ast class_name)
    ((λ (classes state_cont return_cont continue_cont break_cont throw_cont)
       (interpret_ast (state_push_scope (state_build '() '() classes))
                      (cadr (lookup_item (classdef_class_methods
                                          (lookup_item classes
                                                       class_name
                                                       "Undefined class"))
                                         'main
                                         "Undefined entry point"))
                      (λ (state) (call_function state 'main '() state_cont return_cont
                                                continue_cont
                                                break_cont
                                                throw_cont))
                      return_cont
                      continue_cont
                      break_cont
                      throw_cont))
     (interpret_classes ast)
     (λ (v) v); (error "No return statement encountered"))
     (λ (v) (pretty_value v))
     (λ (v) (error "Continue encountered outside of loop"))
     (λ (v) (error "Break encountered outside of loop"))
     (λ (v) (error "Uncaught throw")))))

; Looks up an item in list of key value pairs.
; list: a list of key value pairs.
; name: the key to lookup.
; err_msg: the message to show if the value cannot be found.
(define lookup_item
  (λ (list name err_msg)
    (cond
      ((null? list) (error err_msg name))
      ((eq? (caar list) name) (cdar list))
      (else (lookup_item (cdr list) name err_msg)))))

; Interprets top level AST into a list of classdefs of the format
; (name [classdef]).
; ast: top level abstract syntax tree.
(define interpret_classes
  (λ (ast)
    (if (null? ast)
        '()
        (cons (interpret_class (current_statement ast)) (interpret_classes (remaining_statements ast))))))

; Gets a key value pair of the format (name [class])
; class: the class's ast.
(define interpret_class
  (λ (class)
    (cons (classast_name class) (classdef_build (classast_parent class)
                                                (classast_statements class 'var)
                                                (classast_statements class 'function)
                                                (classast_statements class 'static-var)
                                                (classast_statements class 'static-function)))))

; Gets a list of statements from the classast.
; class: a classast.
; filter: 'function, 'static-function, 'var, or similar.
(define classast_statements
  (λ (class statement_type)
    (map cdr (filter (λ (statement) (eq? (operator statement) statement_type)) (classast_body class)))))

; Gets the name of the class from a class definition.
(define classast_name cadr)

; Gets the name of the parent class of the given class or null for none.
; class: a toplevel class ast of the format (class (extends B) ...)
(define classast_parent
  (λ (class)
    (if (null? (caddr class))
        '()
        (cadr (caddr class)))))

; Gets the list of declarations inside of the body of the class declaration ast.
(define classast_body cadddr)

; Creates a new class definition
; parent_class: the class_def of the parent of this class.
; instance_fields: the instance fields of this class.
; instance_methods: the methods of this class.
; class_fields: static fields.
; class_methods: static methods.
(define classdef_build
  (λ (parent_class instance_fields instance_methods class_fields class_methods)
    (cons parent_class (cons instance_fields (cons instance_methods (cons class_fields (cons class_methods '())))))))

; Gets the parent class from a classdef.
(define classdef_parent_class car)

; Gets the instance fields from a classdef.
(define classdef_instance_fields cadr)

; Gets the methods from a classdef.
(define classdef_instance_methods caddr)

; Gets the class fields from a classdef.
(define classdef_class_fields cadddr)

; Gets the class methods from a classdef.
(define classdef_class_methods
  (λ (class)
    (car (cddddr class))))

; Creates a new class instance.
; class_definition: A class template created by class_definition_build.
; instance_field_values: A list of instance field values.
(define classinst_build
  (λ (class_definition instance_field_values)
    (cons class_definition (cons instance_field_values '()))))

; Gets the classdef from a class instance.
(define classinst_class_definition car)

; Gets the field values from a class instance.
(define classinst_instance_field_values cadr)

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
        (interpret_statement state
                          (current_statement ast)
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
      ((eq? 'var (operator statement)) (state_cont (interpret_var state statement continue_cont break_cont throw_cont)))
      ((eq? '= (operator statement)) (interpret_assign state statement state_cont continue_cont break_cont throw_cont))
      ((eq? 'while (operator statement)) (interpret_while state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'return (operator statement)) (return_cont (value state (operand_1 statement) continue_cont break_cont throw_cont)))
      ((eq? 'if (operator statement)) (interpret_if state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'begin (operator statement)) (interpret_block state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'continue (operator statement)) (continue_cont state))
      ((eq? 'break (operator statement)) (break_cont state))
      ((eq? 'try (operator statement)) (interpret_try state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'throw (operator statement)) (throw_cont (value state (operand_1 statement) continue_cont break_cont throw_cont)))
      ((eq? 'function (operator statement)) (interpret_function state statement state_cont return_cont continue_cont break_cont throw_cont))
      ((eq? 'funcall (operator statement)) (call_function state
                                                          (operand_1 statement)
                                                          (cddr statement) state_cont (λ (v) (state_cont state))
                                                          continue_cont break_cont throw_cont))
      (else (error "invalid statement")))))

; Interprets a function definition.
;
; state: the current program state.
; statement: a properly formed abstract syntax tree statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define interpret_function
  (λ (state statement state_cont return_cont continue_cont break_cont throw_cont)
    (state_cont (state_decl_function state (cadr statement) (cons (state_is_top_level state) (cddr statement))))))

; Calls a function.
; state: the current program state.
; name_expr: either a function name or a (dot ..) expression.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define call_function
  (λ (state name_expr args state_cont return_cont continue_cont break_cont throw_cont)
    ((λ (call_func)
       (if (list? name_expr)
           (resolve_dot state
                        name_expr
                        resolve_member_func
                        call_func
                        continue_cont
                        break_cont
                        throw_cont)
           (call_func null(state_lookup_function state name_expr)))) ;TODO: pass class instance.
     (λ (classinst func)
       (interpret_ast (function_closure state classinst func args continue_cont break_cont throw_cont) (caddr func)
                        state_cont
                        (λ (v) (return_cont v))
                        (λ (v) (error "Continue encountered outside of loop"))
                        (λ (v) (error "Break encountered outside of loop"))
                        throw_cont)))))

; Binds params to their formal params.
; state: the current program state.
; statement: a properly formed abstract syntax tree statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define bind_params
  (λ (current_state new_state formal_args args continue_cont break_cont throw_cont)
    (if (or (null? formal_args) (null? args))
        new_state
        (bind_params current_state (declare_variable new_state (car formal_args) #t
                                                     (value current_state (car args) continue_cont break_cont throw_cont)
                                                     continue_cont break_cont throw_cont)
                     (cdr formal_args) (cdr args) continue_cont break_cont throw_cont))))

; Closes the function state.
; state: the current program state.
; statement: a properly formed abstract syntax tree statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; return_cont: return continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define function_closure
  (λ (state classinst func args continue_cont break_cont throw_cont)
    ((λ (formal_args)
       (if (not (eq? (length formal_args) (length args)))
           (error "Invalid number of arguments in function call")
           (declare_variable
            (bind_params state (state_push_scope (if (car func) ; is topmost
                                                     (state_topmost_state state)
                                                     state))
                         formal_args args continue_cont break_cont throw_cont)
            'this #f classinst continue_cont break_cont throw_cont)))
       (cadr func))))
    

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
                   (λ (v) (interpret_catch state statement
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
                   (λ (v) (throw_cont v)))))

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
                   (λ (v) (throw_cont v)))))

; Interprets a var declaration.
; Throws an error if: variables are declared multiple times.
;
; state: the current program state.
; statement: a properly formed abstract syntax tree block statement.
(define interpret_var
  (λ (state statement continue_cont break_cont throw_cont)
    (declare_variable state
                      (operand_1 statement)
                      (has_operand_2 statement)
                      (if (has_operand_2 statement)
                          (operand_2 statement)
                          null)
                      continue_cont break_cont throw_cont)))

; Declares a variable.
(define declare_variable
  (λ (state name has_val val continue_cont break_cont throw_cont)
    (state_stack_level_replace (car (state_stack state)) name 0
                               (λ () (error "variable already declared:" name))
                               (λ () (state_add state name
                                                (if has_val
                                                    (value state val continue_cont break_cont throw_cont)
                                                    val))))))

; Interprets a var assign statement from the AST and returns the updated state.
; Throws an error if: variable has not yet been declared.
;
; state: a list containing the current state.
; statement: a single parsed assign statement.
(define interpret_assign
  (λ (state statement state_cont continue_cont break_cont throw_cont)
    (value_cps state (operand_2 statement) (λ (v) (state_update state (operand_1 statement) v
                                                                (λ () (state_cont state))
                                                                (λ () (error "undeclared variable in assignment")))) continue_cont break_cont throw_cont)))
    

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
  (λ (state statement state_cont return_cont continue_cont break_cont throw_cont)
    (cond
      ((not (value state (condition statement) continue_cont break_cont throw_cont)) (state_cont state))
      (else (interpret_statement state (true_statement statement) 
                                 (λ (v) (interpret_while v statement state_cont return_cont continue_cont break_cont throw_cont))
                                 return_cont
                                 (λ (v) (interpret_while v statement state_cont return_cont continue_cont break_cont throw_cont))
                                 (λ (v) (state_cont v))
                                 throw_cont)))))

; Interprets an if statement.
; Throws an error if: no return statement in control flow path or
; variables are used before being declared, variables are declared
; multiple times, break or continue statement is encountered outside
; of loop, or a throw statement is outside of a try/catch block.
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
    (if (value state (condition statement) continue_cont break_cont throw_cont)
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
; s: current state list.
; expression: the expression AST.
; Throws error if: variable is used before it is declared or an unknown
; operation is attempted.
; Returns: the value of the expression.
(define value
  (λ (s expression continue_cont break_cont throw_cont)
    (value_cps s expression (λ (v) v) continue_cont break_cont throw_cont)))

; CPS version of value.
; state: the current program state.
; statement: a properly formed abstract syntax tree statement.
; state_cont: State continuation function.
; continue_cont: continue continuation function.
; break_cont: break continuation function.
; throw_cont: throw_continuation function.
(define value_cps
  (λ (s expression state_cont continue_cont break_cont throw_cont)
    (cond
      ((or (number? expression) (boolean? expression)) (state_cont expression))
      ((eq? 'true expression) (state_cont #t))
      ((eq? 'false expression) (state_cont #f))
      ((not (list? expression)) (state_cont (state_value s expression)))
      ((and (list? expression)
            (eq? 'funcall (operator expression)))
       (call_function s
                      (operand_1 expression)
                      (cddr expression)
                      (λ (v) (error "Function did not return a value in expression"))
                      state_cont
                      continue_cont
                      break_cont
                      throw_cont))
      ((and (list? expression) (eq? 'new (operator expression))) (value_new s expression state_cont))
      ((and (list? expression) (eq? 'dot (operator expression)))
       (resolve_dot s expression resolve_field state_cont continue_cont break_cont throw_cont))
      ((not (has_operand_2 expression))
       (value_cps s
                  (operand_1 expression)
                  (λ (v) (state_cont ((operation_function (operator expression)) 0 v)))
                  continue_cont break_cont throw_cont))
      (else (value_cps s
                       (operand_1 expression)
                       (λ (v1) (value_cps s (operand_2 expression)
                                          (λ (v2) (state_cont ((operation_function (operator expression)) v1 v2)))
                                          continue_cont
                                          break_cont
                                          throw_cont))
                       continue_cont break_cont throw_cont)))))

; Evaluates an expression of the form A.B and resolves its value.
; state: The current program state.
; expression: The (dot A B) expression.
; right_func: function to call on the right side of the dot.
; value_cont: called to pass along the value.
; continue_cont: we hit a continue statement.
; break_cont: we hit a break statement.
; throw_cont: we hit a throw statement.
(define resolve_dot
  (λ (state expression right_func value_cont continue_cont break_cont throw_cont)
    (value_cps state
           (cadr expression)
           (λ (left_value)
             (if (not (list? left_value))
                 (error "Left side of dot operator is not an object")
                 (right_func left_value (caddr expression) state value_cont continue_cont break_cont throw_cont)))
           continue_cont
           break_cont
           throw_cont)))

; Evaluates a field.
; left_value: the value of the left side of the dot expression.
; right_expr: the right expression.
; state: the current program state.
; value_cont: called to passalong the value.
; continue_cont: called if continue keyword is encountered.
; break_cont: called if break keyword is encountered.
; throw_cont: called if throw keyword is encountered.
(define resolve_field
  (λ (left_value right_expr state value_cont continue_cont break_cont throw_cont)
    (state_stack_level_value (classinst_instance_field_values left_value)
                             right_expr
                             value_cont
                             (λ ()(error "Undefined field" right_expr)))))

(define resolve_member_func
  (λ (left_value right_expr state value_cont continue_cont break_cont throw_cont)
    (value_cont left_value (cons right_expr (lookup_item (classdef_instance_methods (classinst_class_definition left_value))
                                                         right_expr
                                                         "Undefined member function")))))

; Evaluates a new expression and creates a new object instance.
(define value_new
  (λ (state expression value_cont)
    ((λ (classdef)
       (value_cont (classinst_build classdef
                                    (define_instance_field_values state (classdef_instance_fields classdef) '()))))
     (lookup_item (state_classdefs state)
                  (cadr expression)
                  "Undefined class"))))

; Create a list of field key value pairs.
; state: the current state.
; fields_def: the list of field name/value combinations from the class def.
; fields_inst: the list of existing fields from the instance.
(define define_instance_field_values
  (λ (state fields_def fields_inst)
    (if (null? fields_def)
        fields_inst
        ((λ (name value_expr)
           (define_instance_field_values
             state
             (cdr fields_def)
             (state_stack_level_replace fields_inst
                                        name
                                        value_expr  ;; TODO: evaluate value_expr??
                                        (λ () (error "Duplicate field" name))
                                        (λ () (state_stack_level_add fields_inst name value_expr)))))
         (caar fields_def)
         (cadar fields_def)))))

; Wraps the value function such as to return non-lisp versions
; of boolean values.
(define pretty_value
  (λ (expression)
    (cond
      ((eq? expression #t) 'true)
      ((eq? expression #f) 'false)
      (else expression))))

; Helper functions:
; ==========================================================

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

; Gets the value of a variable in a specific level of the state.
; s: state stack.
; name: the name of the variable.
; return_cont: the return continuation for the value.
; nonfound_cont: the continatuion for values that were not able to be found.
(define state_stack_level_value
  (λ (s name return_cont notfound_cont)
    (cond
      ((null? s) (notfound_cont))
      ((eq? (caar s) name) (return_cont (unbox (cadar s))))
      (else (state_stack_level_value (cdr s) name return_cont notfound_cont)))))

; Iterates the stack of scopes in the state, starting from current and going up
; to find the value of the given var.
; s: the state
; name: the name of the variable.
; Returns the variable.
; Throws: undefined var if it doesn't exist or used before initialization if the
; var was declared without a initial value.
(define state_stack_value
  (λ (stack name)
    (if (null? stack)
        (error "undefined variable or function" name)
        (state_stack_level_value (car stack) name
                           (λ (v) (if (null? v)
                                      (error "variable used before initialization")
                                      v))
                           (λ () (state_stack_value (cdr stack) name))))))

(define state_value
  (λ (state name)
    (state_stack_value (state_stack state) name)))

; Adds the specified value to the state s mapped to the specified variable
; in the current scope.
; Returns: the updated state. This does not remove existing mappings of name.
(define state_add
  (λ (state name value)
    ((λ (stack)
      (state_build (state_functions state)
                   (cons (state_stack_level_add (car stack) name value) (cdr stack))
                   (state_classdefs state)))
     (state_stack state))))

; Adds the specified value to the given level of the state.
; state: a state level.
; name: the name of the variable.
; value: the value to map to the name.
(define state_stack_level_add
  (λ (stack name value)
    (cons (cons name (cons (box value) '())) stack)))

; Iterates through the current level of the scope, heading out
; until it locates the existing mapping for the variable and replaces it.
; stack: the state stack.
; name: the name of the variable.
; value: the new value.
; replaced_cont: the continuation for if the value was replaced.
; notreplaced_cont: the continuation for if the value was not replaced.
(define state_stack_level_replace
  (λ (stack name value replaced_cont notreplaced_cont)
    (cond
      ((null? stack) (notreplaced_cont))
      ((eq? (caar stack) name) (begin (set-box! (cadar stack) value) (replaced_cont)))
      (else (state_stack_level_replace (cdr stack) name value replaced_cont notreplaced_cont)))))

; Iterates through the levels of the scope, starting from current, heading out
; until it locates the existing mapping for the variable and replaces it.
; stack the state stack.
; name: the name of the variable.
; value: the new value.
; updated_cont: the continuation for if the value was replaced.
; notupdated_cont: the continuation for if the value was not replaced.
(define state_stack_update
  (λ (stack name value updated_cont notupdated_cont)
    (cond
      ((null? stack) (notupdated_cont))
      ((null? (car stack)) (state_stack_update (cdr stack) name value updated_cont notupdated_cont))
      (else (state_stack_level_replace (car stack) name value
                                       updated_cont
                                       (λ () (state_stack_update (cdr stack) name value updated_cont notupdated_cont)))))))

; Wrapper for state_stack_update.
(define state_update
  (λ (state name value updated_cont notupdated_cont)
    (state_stack_update (state_stack state) name value updated_cont notupdated_cont)))

; Declares a new function.
(define state_decl_function
  (λ (state name value)
    (state_stack_update (state_functions state) name value
                        (λ () (error "Function with this name already exists"))
                        (λ () ((λ (stack)
                                  (state_build (cons (state_stack_level_add (car stack) name value) (cdr stack))
                                               (state_stack state)
                                               (state_classdefs state)))
                                (state_functions state))))))

(define state_lookup_function
  (λ (state name)
    (state_stack_value (state_functions state) name)))  

; Wrapper for state_stack_push_scope
(define state_push_scope
  (λ (state)
    (state_build (cons '() (state_functions state))
                 (cons '() (state_stack state))
                 (state_classdefs state))))

(define state_pop_scope
  (λ (state)
    (state_build (cdr (state_functions state))
                 (cdr (state_stack state))
                 (state_classdefs state))))

; Gets the stack of state variables and functions.
(define state_stack cadr)

; Gets the function variables from the state.
(define state_functions car)

; Gets the class defs from the state.
(define state_classdefs caddr)

; Builds a new state object.
(define state_build
  (λ (functions stack classdefs)
    (cons functions (cons stack (cons classdefs '())))))

; Checks if the current scope is top level.
; If the current scope is top level, returns #t.
(define state_is_top_level
  (λ (state)
    (null? (cdar state))))

; Gets the last item in a list.
(define last
  (λ (list)
    (if (null? (cdr list))
        (car list)
        (last (cdr list)))))


; Gets a new state containing only the topmost functions and vars.
(define state_topmost_state
  (λ (state)
    (state_build (cons (last (state_functions state)) '())
                 (cons (last (state_stack state)) '())
                 (state_classdefs state))))
