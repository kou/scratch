(define-module scratch.session
  (extend scratch.scratch)
  (use util.list)
  (use srfi-1)
  (use gauche.collection)
  (use marshal)
  (use scratch.common)
  (export make-scratch-session
          response-info-list
          get-value set-value! delete-value! value-exists?
          get-id set-id!
          get-response-value set-response-value!
          delete-response-value! response-value-exists?
          get-cycle-value set-cycle-value!
          delete-cycle-value! cycle-value-exists?
          clear! valid? scratch-session?))
(select-module scratch.session)

(define (make-response-values)
  (make-hash-table 'eq?))

(define (make-cycle-values)
  (make-hash-table 'eq?))

(define (x->keyword x)
  (if (keyword? x)
    x
    (make-keyword (x->string x))))

(define (x->symbol x)
  (if (symbol? x)
    x
    (string->symbol (x->string x))))

(define (keyword->symbol keyword)
  (string->symbol (keyword->string keyword)))

(define-class <scratch-session> ()
  ((values :accessor values-of :init-form (make-hash-table 'eq?))
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

(define-method marshalizable? ((self <scratch-session>))
  #t)

(define-reader-ctor '<scratch-session>
  (lambda (timeout init-values)
    (apply make-scratch-session timeout init-values)))

(define-method write-object ((self <scratch-session>) out)
  (format out "#,(<scratch-session> ~s ~s)"
          (timeout-of self)
          (fold (lambda (elem prev)
                  (if (marshalizable? (cdr elem))
                    (cons (make-keyword (car elem))
                          (cons (cdr elem)
                                prev))
                    prev))
                '()
                (hash-table->alist (values-of self)))))

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
                         (cons (x->keyword key)
                               (cons value prev))
                         prev))
                   (list *scratch-id-key* (get-id self))))

(define-method get-value ((self <scratch-session>) key . default)
  (hash-table-get (values-of self) (x->symbol key)
                  (get-optional default #f)))

(define-method set-value! ((self <scratch-session>) key value)
  (hash-table-put! (values-of self) (x->symbol key) value))

(define-method delete-value! ((self <scratch-session>) key)
  (hash-table-delete! (values-of self) (x->symbol key)))

(define-method value-exists? ((self <scratch-session>) key)
  (hash-table-exists? (values-of self) (x->symbol key)))

(define-method get-id ((self <scratch-session>))
  (get-value self *scratch-id-key* #f))

(define-method set-id! ((self <scratch-session>) id)
  (set-value! self *scratch-id-key* id))

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

(define (scratch-session? session)
  (is-a? session <scratch-session>))

(provide "scratch/session")

;; for id-of in set-id!
(with-module scratch.common
  (use scratch.session))
