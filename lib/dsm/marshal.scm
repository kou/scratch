(define-module dsm.marshal
  (use srfi-1)
  (use srfi-10)
  (use util.list)
  (use gauche.collection)
  (export marshalizable? marshal unmarshal referenced-object?
          ct)
  )
(select-module dsm.marshal)

(define-class <reference-object> ()
  ((id :init-keyword :id :accessor id-of))
  )

(define (referenced-object? obj)
  (is-a? obj <reference-object>))
         
(define-reader-ctor '<reference-object>
  (lambda (id)
    (make <reference-object> :id id)))

(define-method write-object ((self <reference-object>) out)
  (format out "#,(<reference-object> ~s)" (id-of self)))

; (define-method object-hash ((self <reference-object>))
;   (id-of self))

; (define-method object-equal? ((self <reference-object>) other)
;   (and (is-a? other <reference-object>)
;        (= (id-of self) (id-of other))))

; (define-method object-equal? (self (other <reference-object>))
;   (and (is-a? self <reference-object>)
;        (= (id-of self) (id-of other))))

(define-values (id-get id-ref ct)
  (let ((obj->id (make-hash-table 'eq?))
        (id->obj (make-hash-table 'eqv?))
        (cnt 0))
    (values (lambda (obj)
              (if (eq? obj #f)
                  0
                  (or (hash-table-get obj->id obj #f)
                      (begin (inc! cnt)
                             (hash-table-put! obj->id obj cnt)
                             (hash-table-put! id->obj cnt obj)
                             cnt))))
            (lambda (id)
              (if (= id 0)
                  #f
                  (or (hash-table-get id->obj id #f)
                      (error "no object with id: " id))))
            (lambda ()
              (p (hash-table->alist id->obj)))
            )))

(define-method marshalizable? (obj)
  #f)

(define-method marshalizable? ((obj <number>))
  #t)

(define-method marshalizable? ((obj <symbol>))
  #t)

(define-method marshalizable? ((obj <char>))
  #t)

(define-method marshalizable? ((obj <string>))
  #t)

(define-method marshalizable? ((obj <boolean>))
  #t)

(define-method marshalizable? ((obj <keyword>))
  #t)

(define-method marshalizable? ((lst <list>))
  #t)

(define-method marshalizable? ((vec <vector>))
  #t)

(define-method marshal (object)
  (define (make-marshalized-object obj)
    (if (and (marshalizable? obj)
             (is-a? obj <collection>))
        (map-to (class-of obj)
                make-marshalized-object
                obj)
        (if (marshalizable? obj)
            obj
            (make <reference-object> :id (id-get obj)))))

  (let ((out (open-output-string)))
    (write (make-marshalized-object object) out)
    (get-output-string out)))

(define-method unmarshal (obj)
  (if (is-a? obj <collection>)
      (map-to (class-of obj)
              unmarshal
              obj)
      (if (is-a? obj <reference-object>)
          (id-ref (id-of obj))
          obj)))

(provide "dsm/marshal")