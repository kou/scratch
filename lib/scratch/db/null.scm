(define-module scratch.db.null
  (extend scratch.db)
  (export <scratch-db-null>))
(select-module scratch.db.null)

(define-class <scratch-db-null> ()
  ())

(define-method store ((self <scratch-db-null>) base-dir)
  #f)

(define-method restore ((self <scratch-db-null>) base-dir)
  #f)

(define-method get-value ((self <scratch-db-null>) key . default)
  (get-optional default #f))
  
(define-method set-value! ((self <scratch-db-null>) key value)
  #f)
  
(define-method remove-value! ((self <scratch-db-null>) key)
  #f)

(define-method value-exists! ((self <scratch-db-null>) key)
  #f)

(provide "scratch/db/null")