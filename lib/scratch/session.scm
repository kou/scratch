(define-module scratch.session
  (use util.list)
  (use scratch.common)
  (export make-scratch-session
          id-of response-info-list
          get-value set-value! delete-value! value-exists?
          get-response-value set-response-value!
          delete-response-value! response-value-exists?
          get-cycle-value set-cycle-value!
          delete-cycle-value! cycle-value-exists?
          clear! valid?))
(select-module scratch.session)

(define (make-response-values)
  (make-hash-table 'eq?))

(define (make-cycle-values)
  (make-hash-table 'eq?))

(define-class <scratch-session> ()
  ((id :accessor id-of)
   (values :accessor values-of :init-form (make-hash-table 'eq?))
   (response-values :accessor response-values-of
                    :init-thunk make-response-values)
   (cycle-values :accessor cycle-values-of
                 :init-thunk make-cycle-values)
   (timeout :accessor timeout-of
            :init-keyword :timeout
            :init-value 3600)
   (constructed-time :accessor constructed-time-of
                     :init-thunk sys-time)
   ))

(define (make-scratch-session . init-values)
  (define (keyword->symbol keyword)
    (string->symbol (keyword->string keyword)))
  (define (compute-session-args)
    (if (and (not (null? init-values))
             (or (eq? #f (car init-values))
                 (integer? (car init-values))))
        (begin0
            (list :timeout (car init-values))
          (set! init-values (cdr init-values)))
        '()))
  (let ((session (apply make <scratch-session> (compute-session-args))))
    (for-each (lambda (elem)
                (let ((key (car elem))
                      (value (cadr elem)))
                  (set-value! session (keyword->symbol key) value)))
              (slices init-values 2))
    session))

(define-method valid? ((self <scratch-session>))
  (if (timeout-of self)
      (> 0 (- (sys-time)
              (+ (constructed-time-of self) (timeout-of self))))
      #t))

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

(define-method get-cycle-value ((self <scratch-session>) key . default)
  (hash-table-get (cycle-values-of self) key
                  (get-optional default #f)))

(define-method set-cycle-value! ((self <scratch-session>) key value)
  (hash-table-put! (cycle-values-of self) key value))

(define-method delete-cycle-value! ((self <scratch-session>) key)
  (hash-table-delete! (cycle-values-of self) key))

(define-method cycle-value-exists? ((self <scratch-session>) key)
  (hash-table-exists? (cycle-values-of self) key))

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
  (set! (cycle-values-of self) (make-cycle-values)))

(provide "scratch/session")

;; for id-of in set-id!
(with-module scratch.common
  (use scratch.session))
