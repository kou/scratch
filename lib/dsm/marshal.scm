(define-module dsm.marshal
  (use srfi-1)
  (use srfi-10)
  (use util.list)
  (use gauche.collection)
  (export marshalizable? marshal unmarshal referenced-object?
          ct
          make-marshal-table)
  )
(select-module dsm.marshal)

(define-class <reference-object> ()
  ((id :init-keyword :id :accessor id-of)
   (host :init-keyword :host :accessor host-of)
   (port :init-keyword :port :accessor port-of))
  )

(define (referenced-object? obj)
  (is-a? obj <reference-object>))
         
(define-reader-ctor '<reference-object>
  (lambda (id host port)
    (make <reference-object> :id id :host host :port port)))

(define-method write-object ((self <reference-object>) out)
  (format out "#,(<reference-object> ~s ~s ~s)"
          (id-of self)
          (host-of self)
          (port-of self)
          ))

; (define-method object-hash ((self <reference-object>))
;   (id-of self))

; (define-method object-equal? ((self <reference-object>) other)
;   (and (is-a? other <reference-object>)
;        (= (id-of self) (id-of other))))

; (define-method object-equal? (self (other <reference-object>))
;   (and (is-a? self <reference-object>)
;        (= (id-of self) (id-of other))))

(define (make-marshal-table host port)
  (let ((obj->id (make-hash-table 'eq?))
        (id->obj (make-hash-table 'eqv?))
        (cnt 0))
    (lambda (command . arg)
      (apply
       (case command
         ((get)
          (lambda (obj)
            (if (eq? obj #f)
                0
                (or (hash-table-get obj->id obj #f)
                    (begin (inc! cnt)
                           (hash-table-put! obj->id obj cnt)
                           (hash-table-put! id->obj cnt obj)
                           cnt)))))
         ((ref)
          (lambda (id)
            (if (= id 0)
                #f
                (or (hash-table-get id->obj id #f)
                    (error "no object with id: " id)))))
         ((ct)
          (lambda ()
            (hash-table->alist id->obj)))
         ((host)
          (lambda () host))
         ((port)
          (lambda () port)))
       arg))))

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

(define (marshal table object)
  (define (make-marshalized-object obj)
    (if (and (marshalizable? obj)
             (is-a? obj <collection>))
        (map-to (class-of obj)
                make-marshalized-object
                obj)
        (if (marshalizable? obj)
            obj
            (make <reference-object>
              :id (table 'get obj)
              :host (table 'host)
              :port (table 'port)))))

  (let ((out (open-output-string)))
    (write (make-marshalized-object object) out)
    (get-output-string out)))

(define (unmarshal table object)
  (if (is-a? object <collection>)
      (map-to (class-of object)
              (cut unmarshal table <>)
              object)
      (if (is-a? object <reference-object>)
          (table 'ref (id-of object))
          object)))

(provide "dsm/marshal")