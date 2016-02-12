(load "simpleParser.scm")

;Start interpreting a file
(define start
  (lambda (filename)
    (interpret_all '() (parser filename))))

;Interpret a list of commands
(define interpret_all
  (lambda (state commands)
    (display (caar commands))
    (cond
      ((null? commands) (error "no commands"))
      ((eq? 'return (caar commands)) (M_value state cadr))
      (else 
        (interpret_all (interpret state (car commands)) (cdr commands))))))

;Interpret a single command and return the new state
(define interpret
  (lambda (state command)
    (cond
      ((eq? 'var (operator command)) (interpret_setVar state command))
      ((eq? 'while (operator command)) (interpret_while state command))
      ((eq? 'if (operator command)) (interpret_if state command)))))
      

(define operator
  (lambda (expression)
    (car expression)))

(define operand_1
  (lambda (expression)
    (cadr expression)))

(define operand_2
  (lambda (expression)
    (caddr expression)))

;; Looks up an arithmetic function by its symbol.
(define operation_function
  (lambda (operator)
    (cond
      ((eq? '+ operator) (lambda (a b) (+ a b)))
      ((eq? '- operator) (lambda (a b) (- a b)))
      ((eq? '* operator) (lambda (a b) (* a b)))
      ((eq? '/ operator) (lambda (a b) (quotient a b)))
      ((eq? '% operator) (lambda (a b) (remainder a b)))
      ((eq? 'return operator) (lambda (a) (M_value a)))
      ((eq? '< operator) (lambda (a b) (< a b)))
      ((eq? '> operator) (lambda (a b) (> a b)))
      ((eq? '<= operator) (lambda (a b) (<= a b)))
      ((eq? '>= operator) (lambda (a b) (>= a b)))
      ((eq? '== operator) (lambda (a b) (eq? a b)))
      ((eq? '!= operator) (lambda (a b) (not (eq? a b))))
      ((eq? '&& operator) (lambda (a b) (and a b)))
      ((eq? '|| operator) (lambda (a b) (or a b)))
      )))


;; Gets the value of an expression.
(define M_value
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((eq? '! (operator expression)) (not (M_value (operand_1 expression))))
      (else ((operation_function (operator expression))
             (M_value (operand_1 expression))
             (M_value (operand_2 expression)))))))