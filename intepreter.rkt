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

(define has_operand_2
  (lambda (expression)
    (not (null? (cddr expression)))))

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
(define M_value
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (has_operand_2 expression))((operation_function (operator expression))
             0
             (M_value (operand_1 expression))))
      (else ((operation_function (operator expression))
             (M_value (operand_1 expression))
             (M_value (operand_2 expression)))))))