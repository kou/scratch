(define-module scratch.session
  (use util.list)
  (export make-scratch-session
          id-of response-key-of response-info
          id-set!
          get-value set-value! key-exists?
          get-response-value set-response-value! response-key-exists?
          ))
(select-module scratch.session)

(define-class <scratch-session> ()
  ((id :accessor id-of)
   (response-key :accessor response-key-of :init-value #f)
   (response-info :accessor response-info-of
                  :init-form (make-hash-table 'eq?))
   (values :accessor values-of :init-form (make-hash-table 'eq?))
   ))

; (define-method id-set! ((self <scratch-session>) value)
;   (set! (id-of self) value))

(define (id-set! session value)
  (slot-set! session 'id value))

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

(define-method response-info ((self <scratch-session>))
  (hash-table-fold (response-info-of self)
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
  (apply hash-table-get (values-of self) key default))

(define-method set-value! ((self <scratch-session>) key value)
  (hash-table-put! (values-of self) key value))

(define-method key-exists? ((self <scratch-session>) key)
  (hash-table-exists? (values-of self) key))

(define-method get-response-value ((self <scratch-session>) key . default)
  (apply hash-table-get (response-info-of self) key default))

(define-method set-response-value! ((self <scratch-session>) key value)
  (hash-table-put! (response-info-of self) key value))

(define-method response-key-exists? ((self <scratch-session>) key)
  (hash-table-exists? (response-info-of self) key))

(provide "scratch/servlet")