(define-module scratch.db
  (extend scratch.common)
  (export store restore
          get-value set-value! remove-value! value-exists?))
(select-module scratch.db)

(define-class <scratch-db> ()
  ())

(define-method store ((self <scratch-db>))
  (error "not implemented"))

(define-method restore ((self <scratch-db>))
  (error "not implemented"))

(define-method get-value ((self <scratch-db>) key . default)
  (error "not implemented"))
  
(define-method set-value! ((self <scratch-db>) key value)
  (error "not implemented"))
  
(define-method remove-value! ((self <scratch-db>) key)
  (error "not implemented"))
  
(define-method value-exists? ((self <scratch-db>) key)
  (error "not implemented"))
  
(provide "scratch/db")