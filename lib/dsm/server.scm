(define-module dsm.server
  (use srfi-13)
  (use gauche.net)
  (use gauche.selector)
  (use dsm.marshal)
  (use dsm.common)
  (export make-dsm-server start-dsm-server stop-dsm-server
          add-mount-point! get-by-mount-point)
  )
(select-module dsm.server)

(define-class <dsm-server> ()
  ((host :init-keyword :host :accessor host-of :init-value #f)
   (port :init-keyword :port :accessor port-of :init-value 59102)
   (mount-table :accessor mount-table-of)
   (socket :accessor socket-of)
   (marshal-table :accessor marshal-table-of)
   ))

(define-method initialize ((self <dsm-server>) . args)
  (next-method)
  (slot-set! self 'mount-table (make-hash-table 'string=?))
  (slot-set! self 'socket (make-server-socket 'inet
                                              (port-of self)
                                              :reuse-addr? #t))
  (slot-set! self 'marshal-table
             (apply make-marshal-table
                    (get-sock-host&port (socket-of self))))
  )

(define (make-dsm-server . keywords)
  (apply make <dsm-server> keywords))

(define-method add-mount-point! ((self <dsm-server>) mount-point value)
  (hash-table-put! (mount-table-of self)
                   (x->string mount-point)
                   value))

(define-method get-by-mount-point ((self <dsm-server>) mount-point)
  (hash-table-get (mount-table-of self)
                  (x->string mount-point)))

(define-method start-dsm-server ((self <dsm-server>))
  (let ((selector (make <selector>)))

    (define (accept-handler sock flag)
      (let* ((client (socket-accept (socket-of self)))
             (output (socket-output-port client)))
        (selector-add! selector
                       (socket-input-port client :buffered? #f)
                       (lambda (input flag)
                         (handle-dsmp client input output))
                       '(r))))
    
    (define (handle-dsmp client input output)
      (let ((header (read-line input)))
        (if (eof-object? header)
            (begin (selector-delete! selector input #f #f)
                   (socket-close client))
            (dsmp-response header
                           (marshal-table-of self)
                           input
                           output
                           (cut get-by-mount-point self <>)))))
    
    (selector-add! selector
                   (socket-fd (socket-of self))
                   accept-handler
                   '(r))
    (do () (#f) (selector-select selector))))

(define-method stop-dsm-server ((self <dsm-server>))
  (socket-close (socket-of self)))

(provide "dsm/server")