(define-module where.servlet
  (use scratch.servlet)
  (export make-where-servlet))
(select-module where.servlet)

(define (make-where-servlet)
  (make <scratch-servlet>
    :servlet-module-name 'where.servlet))

(provide "where.servlet")
