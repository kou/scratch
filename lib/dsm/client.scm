(define-module dsm.client
  (use srfi-13)
  (use gauche.net)
  (use dsm.marshal)
  (use dsm.common)
  (export connect-server)
  )
(select-module dsm.client)

(define-class <dsm-client> ()
  ((host :init-keyword :host :accessor host-of :init-value "localhost")
   (port :init-keyword :port :accessor port-of :init-value 59102)
   (socket :accessor socket-of)
   ))

(define-method initialize ((self <dsm-client>) . args)
  (next-method)
  (slot-set! self 'socket
             (make-client-socket 'inet
                                 (host-of self)
                                 (port-of self))))

(define (get-from-remote obj in out . options)
  (let-optionals* options ((command "get"))
    (display (x->dsm-header->string obj :command command) out)
    (display obj out)
    (flush out)
    (let* ((dsm-header (parse-dsm-header (read-line in)))
           (remote-obj (read-from-string
                        (read-block (length-of dsm-header) in))))
      (if (referenced-object? remote-obj)
          (lambda arg
            (eval-in-remote remote-obj arg in out))
          remote-obj))))

(define (get-from-remote obj in out . options)
  (let-optionals* options ((command "get"))
    (dsmp-request obj in out
                  (lambda (header object)
                    (if (referenced-object? object)
                        (lambda arg
                          (eval-in-remote object arg in out))
                        object))
                  :command command)))

(define (get-by-mount-point client-socket mount-point)
  (let ((in (socket-input-port client-socket))
        (out (socket-output-port client-socket)))
    (get-from-remote mount-point in out)))

(define (eval-in-remote obj arg in out)
  (get-from-remote (cons obj arg) in out "eval"))

(define (connect-server . keywords)
  (let ((remote (apply make <dsm-client> keywords)))
    (lambda (mount-point)
      (get-by-mount-point (socket-of remote) mount-point))))

(provide "dsm/client")