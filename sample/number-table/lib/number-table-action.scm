(define-module number-table-action
  (use scratch.servlet)
  (use number-table)
  (export make-number-table-servlet
          do-move))
(select-module number-table-action)

(define (make-session)
  (let ((session (make-scratch-session))
        (table (make-number-table 3)))
    (shuffle-table! table)
    (set-value! session 'table table)
    (set-value! session 'count 0)
    (update-session session)
    session))

(define (get-available-ways table)
  (call-with-values
      (lambda () (available-ways table))
    list))

(define (make-number-table-servlet)
  (make <scratch-servlet>
    :module (find-module 'number-table-action)
    :session-constructor make-session))

(define (table-of session)
  (get-value session 'table))

(define (count-of session)
  (get-value session 'count))

(define (update-session session)
  (let ((table (table-of session)))
    (set-value! session 'clear? (clear? table))
    (set-value! session 'available-ways (get-available-ways table))
    ))

(define (do-move session args)
  (let ((table (table-of session))
        (count (count-of session))
        (way (get-keyword :way args)))
    (set-value! session 'count (+ 1 (count-of self)))
    (move! table (make-keyword way))
    (update-session session)))

(provide "number-table-action")