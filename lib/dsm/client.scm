(define-module dsm.client
  (use gauche.net)
  (use dsm.marshal)
  (export connect-server)
  )
(select-module dsm.client)

(define-class <dsm-client> ()
  ((host :init-keyword :host :accessor host-of :init-value "localhost")
   (port :init-keyword :port :accessor port-of :init-value 59102)
   (sock)
   ))

(define-method initialize ((self <dsm-client>) . args)
  (next-method)
  (slot-set! self 'sock
             (make-client-socket 'inet
                                 (host-of self)
                                 (port-of self))))

(define (connect-server . keywords)
  (let ((server (apply make <dsm-client> keywords)))
    (lambda (key)
      (get server key))))

(provide "dsm/client")