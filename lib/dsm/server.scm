(define-module dsm.server
  (use gauche.net)
  (use gauche.selector)
  (use dsm.marshal)
  (export make-dsm-server start-server
          add-mount-point! get-by-mount-point)
  )
(select-module dsm.server)

(define-class <dsm-server> ()
  ((host :init-keyword :host :accessor host-of :init-value #f)
   (port :init-keyword :port :accessor port-of :init-value 59102)
   (mount-table :accessor mount-table-of)
   (socket :accessor socket-of)
   ))

(define-method initialize ((self <dsm-server>) . args)
  (next-method)
  (slot-set! self 'mount-table (make-hash-table 'string=?))
  (let* ((address (car (make-sockaddrs (host-of self) (port-of self))))
	 (socket (make-socket (with-module gauche.net
                                           (address->protocol-family address))
			      |SOCK_STREAM|)))
    (socket-setsockopt socket |SOL_SOCKET| |SO_REUSEADDR| 1)
    (slot-set! self 'socket socket)
    (socket-bind socket address)
    (socket-listen (socket-of self) 5)))

(define (make-dsm-server . keywords)
  (apply make <dsm-server> keywords))

(define-method add-mount-point! ((self <dsm-server>) mount-point value)
  (hash-table-put! (mount-table-of self)
                   (x->string mount-point)
                   value))

(define-method get-by-mount-point ((self <dsm-server>) mount-point)
  (hash-table-get (mount-table-of self)
                  (x->string mount-point)))

(define-method start-server ((self <dsm-server>))
  (let ((selector (make <selector>)))

    (define (accept-handler sock flag)
      (let* ((client (socket-accept (socket-of self)))
             (output (socket-output-port client)))
        (selector-add! selector
                       (socket-input-port client :buffered? #f)
                       (lambda (input flag)
                         (echo client input output))
                       '(r))))

    (define (echo client input output)
      (let ((str (read-block 4096 input)))
        (if (eof-object? str)
            (begin (selector-delete! selector input #f #f)
                   (socket-close client))
            (begin (display (marshal (get-by-mount-point self str))
                            output)
                   (flush output)))))
    
    (selector-add! selector
                   (socket-fd (socket-of self))
                   accept-handler
                   '(r))
    (do () (#f) (selector-select selector))))

(provide "dsm/server")