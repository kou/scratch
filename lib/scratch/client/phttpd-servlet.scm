(define-module scratch.client.phttpd-servlet
  (extend scratch.client.cgi)
  (export make-scratch-phttpd-servlet)
  )
(select-module scratch.client.phttpd-servlet)

(define (make-scratch-phttpd-servlet client mount-point . servlet-args)
  (lambda (args)
    (apply scratch-cgi-main client mount-point servlet-args)))

(provide "scratch/client/phttpd-servlet")
