(load "simpleParser.scm")

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
(define numeric_operation_function
  (lambda (operator)
    (cond
      ((eq? '+ operator) (lambda (a b) (+ a b)))
      ((eq? '- operator) (lambda (a b) (- a b)))
      ((eq? '* operator) (lambda (a b) (* a b)))
      ((eq? '/ operator) (lambda (a b) (quotient a b)))
      ((eq? '% operator) (lambda (a b) (remainder a b)))
      ((eq? 'return operator) (lambda (a) (M_value a)))
      )))

;; Looks up a boolean operation function by its symbol.
(define boolean_operation_function
  (lambda (operator)
    (cond
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
      (else ((numeric_operation_function (operator expression))
             (M_value (operand_1 expression))
             (M_value (operand_2 expression)))))))

; Gets the value of a boolean expression.
(define M_boolean
  (lambda (expression)
    (cond
      ((eq? true expression) #t)
      ((eq? false expression) #f)
      ((eq? '! (operator expression)) (not (operand_1 expression)))
      (else ((boolean_operation_function (operator expression))
             (M_value (operand_1 expression))
             (M_value (operand_2 expression)))))))