(define-module dsm.client
  (use srfi-13)
  (use gauche.net)
  (use dsm.marshal)
  (use dsm.common)
  (export connect-server)
  )
(select-module dsm.client)

(define-class <dsm-client> ()
  ((host :init-keyword :host :accessor host-of :init-value #f)
   (port :init-keyword :port :accessor port-of :init-value 59102)
   (socket :accessor socket-of)
   ))

(define-method initialize ((self <dsm-client>) . args)
  (next-method)
  (slot-set! self 'socket
             (make-client-socket 'inet
                                 (host-of self)
                                 (port-of self))))

(define (get-from-remote obj table in out . options)
  (define (get-handler obj)
    (if (reference-object? obj)
        (lambda arg
          (eval-in-remote obj arg table in out get-handler))
        obj))

  (apply get-dsm-object-from-remote
         (marshal table obj) table in out
         get-handler
         options))

(define (get-by-mount-point mount-point dsm-client)
  (let* ((client-socket (socket-of dsm-client))
         (in (socket-input-port client-socket))
         (out (socket-output-port client-socket)))
    (get-from-remote mount-point
                     (make-marshal-table-using-socket client-socket)
                     in out)))

(define (connect-server . keywords)
  (let ((remote (apply make <dsm-client> keywords)))
    (lambda (mount-point)
      (get-by-mount-point mount-point remote))))

(provide "dsm/client")