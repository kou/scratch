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
    (p obj)
    (let* ((dsm-header (parse-dsm-header (read-line in)))
           (remote-obj (read-from-string
                        (read-block (length-of dsm-header) in))))
      (if (referenced-object? remote-obj)
          (lambda arg
            (eval-in-remote remote-obj arg in out))
          remote-obj))))

(define (get-from-remote obj table in out . options)
  (apply get-dsm-object-from-remote (marshal table obj) in out options))

(define (get-by-mount-point mount-point client-socket table)
  (let ((in (socket-input-port client-socket))
        (out (socket-output-port client-socket)))
    (get-from-remote mount-point table in out)))

(define (connect-server . keywords)
  (let ((remote (apply make <dsm-client> keywords)))
    (lambda (mount-point)
      (get-by-mount-point mount-point
                          (socket-of remote)
                          (apply make-marshal-table
                                 (get-sock-host&port (socket-of remote)))))))

(provide "dsm/client")