(define-module number-table-servlet
  (use scratch.servlet)
  (use scratch.session)
  (use scratch.user.manager.file)
  (use number-table)
  (use www.cgi)
  (export make-number-table-servlet
          do-move))
(select-module number-table-servlet)

(define (make-number-table-servlet)
  (make <scratch-servlet>
    :servlet-module (find-module 'number-table-servlet)
    :session-constructor make-session
    :user-manager (make <user-manager-file>
                    :default-authority 'deny)))

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

(define (do-move)
  (let* ((sess (session))
         (table (table-of sess))
         (count (count-of sess))
         (way (get-param "way" :convert string->symbol)))
    (set-value! sess 'count (+ 1 (count-of sess)))
    (move! table way)
    (update-session sess)))

(provide "number-table-servlet")