(define-module dsm.server
  (use gauche.net)
  (use gauche.selector)
  (use dsm.marshal)
  (export make-server start-server)
  )
(select-module dsm.server)

(define-class <dsm-server> ()
  ((host :init-keyword :host :accessor host-of :init-value #f)
   (port :init-keyword :port :accessor port-of :init-value 59102)
   (socket :accessor socket-of)
   ))

(define-method initialize ((self <dsm-server>) . args)
  (next-method)
  (let* ((address (car (make-sockaddrs (host-of self) (port-of self))))
	 (socket (make-socket (with-module gauche.net
                                           (address->protocol-family address))
			      |SOCK_STREAM|)))
    (socket-setsockopt socket |SOL_SOCKET| |SO_REUSEADDR| 1)
    (slot-set! self 'socket socket)
    (socket-bind socket address)))

(define (make-server . keywords)
  (apply make <dsm-server> keywords))

(define-method start-server ((self <dsm-server>))
  (socket-listen (socket-of self) 5))

(provide "dsm/server")