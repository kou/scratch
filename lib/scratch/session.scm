(define-module scratch.session
  (use util.list)
  (export make-scratch-session
          id-of response-key-of response-info-list
          get-value set-value! delete-value! value-exists?
          get-response-value set-response-value!
          delete-response-value! response-value-exists?
          get-session-value set-session-value!
          delete-session-value! session-value-exists?
          clear!))
(select-module scratch.session)

(define (make-response-values)
  (make-hash-table 'eq?))

(define (make-session-values)
  (make-hash-table 'eq?))

(define-class <scratch-session> ()
  ((id :accessor id-of)
   (response-key :accessor response-key-of :init-value #f)
   (values :accessor values-of :init-form (make-hash-table 'eq?))
   (response-values :accessor response-values-of
                    :init-thunk make-response-values)
   (session-values :accessor session-values-of
                   :init-thunk make-session-values)
   ))

(define (make-scratch-session . init-values)
  (define (keyword->symbol keyword)
    (string->symbol (keyword->string keyword)))
  (let ((session (make <scratch-session>)))
    (for-each (lambda (elem)
                (let ((key (car elem))
                      (value (cadr elem)))
                  (set-value! session (keyword->symbol key) value)))
              (slices init-values 2))
    session))

(define-method response-info-list ((self <scratch-session>))
  (hash-table-fold (response-values-of self)
                   (lambda (key value prev)
                     (if (or (keyword? key)
                             (symbol? key)
                             (string? key))
                         (cons (if (keyword? key)
                                   key
                                   (make-keyword key))
                               (cons value prev))
                         prev))
                   '()))

(define-method get-value ((self <scratch-session>) key . default)
  (hash-table-get (values-of self) key
                  (get-optional default #f)))

(define-method set-value! ((self <scratch-session>) key value)
  (hash-table-put! (values-of self) key value))

(define-method delete-value! ((self <scratch-session>) key)
  (hash-table-delete! (values-of self) key))

(define-method value-exists? ((self <scratch-session>) key)
  (hash-table-exists? (values-of self) key))

(define-method get-session-value ((self <scratch-session>) key . default)
  (hash-table-get (session-values-of self) key
                  (get-optional default #f)))

(define-method set-session-value! ((self <scratch-session>) key value)
  (hash-table-put! (session-values-of self) key value))

(define-method delete-session-value! ((self <scratch-session>) key)
  (hash-table-delete! (session-values-of self) key))

(define-method session-value-exists? ((self <scratch-session>) key)
  (hash-table-exists? (session-values-of self) key))

(define-method get-response-value ((self <scratch-session>) key . default)
  (hash-table-get (response-values-of self) key
                  (get-optional default #f)))

(define-method set-response-value! ((self <scratch-session>) key value)
  (hash-table-put! (response-values-of self) key value))

(define-method delete-response-value! ((self <scratch-session>) key)
  (hash-table-delete! (response-values-of self) key))

(define-method response-value-exists? ((self <scratch-session>) key)
  (hash-table-exists? (response-values-of self) key))

(define-method clear! ((self <scratch-session>))
  (set! (response-values-of self) (make-response-values))
  (set! (session-values-of self) (make-session-values)))
  
(provide "scratch/session")