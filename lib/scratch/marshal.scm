(define-module scratch.marshal
  (use srfi-1)
  (export marshalizable? marshal unmarshal)
  )
(select-module scratch.marshal)

(define-method marshalizable? (obj)
  #f)

(define-method marshalizable? ((obj <number>))
  #t)

(define-method marshalizable? ((obj <symbol>))
  #t)

(define-method marshalizable? ((obj <string>))
  #t)

(define-method marshalizable? ((lst <list>))
  (fold (lambda (obj ret) (and ret (marshalizable? obj)))
        #t
        lst))

(define-method marshal (obj)
  (if (marshalizable? obj)
      (write-to-string obj)
      (else (error #`",obj can't marshal"))))

(define-method unmarshal (str)
  (read-from-string str))

(provide "scratch/marshal")