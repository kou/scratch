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
   (timeout :init-keyword :timeout :accessor timeout-of
            :init-value '(1 0))
   ))

(define-method initialize ((self <dsm-server>) . args)
  (next-method)
  (slot-set! self 'mount-table (make-hash-table 'string=?))
  (slot-set! self 'socket (make-server-socket 'inet
                                              (port-of self)
                                              :reuse-addr? #t))
  (slot-set! self 'marshal-table
             (make-marshal-table-using-socket (socket-of self)))
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
      (dsmp-response (marshal-table-of self)
                     input
                     output
                     (cut get-by-mount-point self <>)))
    
    (selector-add! selector
                   (socket-fd (socket-of self))
                   accept-handler
                   '(r))
    (do () ((eq? 'shutdown (socket-status (socket-of self))))
      (selector-select selector (timeout-of self)))))

(define-method stop-dsm-server ((self <dsm-server>))
  (socket-shutdown (socket-of self) 2))

(provide "dsm/server")