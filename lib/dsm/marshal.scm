(define-module dsm.marshal
  (use srfi-1)
  (use srfi-10)
  (use util.list)
  (use math.mt-random)
  (use gauche.collection)
  (export marshalizable? reference-object? using-same-table?
          marshal unmarshal id-get id-ref
          make-marshal-table)
  )
(select-module dsm.marshal)

(define mt-random (make <mersenne-twister> :seed (sys-time)))
(define (random)
  (mt-random-real (make <mersenne-twister>)))

(define-class <reference-object> ()
  ((ref :init-keyword :ref :accessor ref-of)
   (id :init-keyword :id :accessor id-of)
   (host :init-keyword :host :accessor host-of)
   (port :init-keyword :port :accessor port-of))
  )

(define (reference-object? obj)
  (is-a? obj <reference-object>))
         
(define-reader-ctor '<reference-object>
  (lambda (ref id host port)
    (make <reference-object> :ref ref :id id :host host :port port)))

(define-method write-object ((self <reference-object>) out)
  (format out "#,(<reference-object> ~s ~s ~s ~s)"
          (ref-of self)
          (id-of self)
          (host-of self)
          (port-of self)
          ))

(define-method object-hash ((self <reference-object>))
  (logior (hash (ref-of self))
          (hash (id-of self))
          (hash (host-of self))
          (hash (port-of self))))

(define (equal-reference-object? ref1 ref2)
  (and (= (ref-of ref1) (ref-of ref2))
       (= (id-of ref1) (id-of ref2))
       (string=? (host-of ref1) (host-of ref2))
       (= (port-of ref1) (port-of ref2))))

(define-method object-equal? ((self <reference-object>) other)
  (and (is-a? other <reference-object>)
       (equal-reference-object? self other)))

(define-method object-equal? (self (other <reference-object>))
  (and (is-a? other <reference-object>)
       (equal-reference-object? self other)))

(define-class <marshal-table> ()
  ((id :accessor id-of)
   (obj->id :accessor obj->id-of)
   (id->obj :accessor id->obj-of)
   (counter :accessor counter-of)
   (host :init-keyword :host :accessor host-of)
   (port :init-keyword :port :accessor port-of))
  )

(define-method initialize ((self <marshal-table>) . args)
  (next-method)
  (slot-set! self 'id (random))
  (slot-set! self 'obj->id (make-hash-table 'eq?))
  (slot-set! self 'id->obj (make-hash-table 'eqv?))
  (slot-set! self 'counter 0))

(define (make-marshal-table host port)
  (make <marshal-table> :host host :port port))

(define-method update-counter! ((table <marshal-table>))
  (inc! (counter-of table)))

(define false-id 0)

(define-method id-get ((table <marshal-table>) obj)
  (if (eq? obj #f)
      false-id
      (or (hash-table-get (obj->id-of table) obj #f)
          (begin (update-counter! table)
                 (hash-table-put! (obj->id-of table) obj (counter-of table))
                 (hash-table-put! (id->obj-of table) (counter-of table) obj)
                 (counter-of table)))))

(define-method id-ref ((table <marshal-table>) id)
  (if (= id false-id)
      #f
      (or (hash-table-get (id->obj-of table) id #f)
          (error "no object with id: " id))))

(define-method ct ((table <marshal-table>)) ;; for debug
  (hash-table->alist (id->obj-of table)))

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

(define (using-same-table? table object)
  (and (reference-object? object)
       (string=? (host-of table) (host-of object))
       (= (port-of table) (port-of object))))

(define (make-reference-object-from-marshal-table table obj)
  (make <reference-object>
    :ref (id-get table obj)
    :id (id-of table)
    :host (host-of table)
    :port (port-of table)))

(define (marshal table object)
  (define (make-marshalized-object obj)
    (if (and (marshalizable? obj)
             (is-a? obj <collection>))
        (map-to (class-of obj)
                make-marshalized-object
                obj)
        (if (or (marshalizable? obj)
                (and (reference-object? obj)
                     (not (using-same-table? table obj))))
            obj
            (make-reference-object-from-marshal-table table obj))))

  (let ((out (open-output-string)))
    (write (make-marshalized-object object) out)
    (get-output-string out)))

(define (unmarshal table object)
  (define (rec obj)
    (if (is-a? obj <collection>)
        (map-to (class-of obj)
                rec
                obj)
        (if (using-same-table? table obj)
            (id-ref table (ref-of obj))
            obj)))

  (rec object))

(provide "dsm/marshal")