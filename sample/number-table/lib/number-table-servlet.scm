(define-module number-table-servlet
  (use scratch.servlet)
  (use scratch.session)
  (use number-table)
  (use www.cgi)
  (export make-number-table-servlet
          do-move))
(select-module number-table-servlet)

(define (make-number-table-servlet)
  (make <scratch-servlet>
    :module (find-module 'number-table-servlet)
    :session-constructor make-session))

(define (make-session)
  (let ((table (make-number-table 3)))
    (shuffle-table! table)
    (let ((session (make-scratch-session :table table
                                         :count 0)))
      (update-session session)
      session)))

(define (get-available-ways table)
  (call-with-values
      (lambda () (available-ways table))
    list))

(define (table-of session)
  (get-value session 'table))

(define (count-of session)
  (get-value session 'count))

(define (update-session session)
  (let ((table (table-of session)))
    (set-value! session 'clear? (clear? table))
    (set-value! session 'available-ways (get-available-ways table))
    ))

(define (do-move session params)
  (let ((table (table-of session))
        (count (count-of session))
        (way (get-param "way" params :convert string->symbol)))
    (set-value! session 'count (+ 1 (count-of session)))
    (move! table way)
    (update-session session)))

(provide "number-table-servlet")