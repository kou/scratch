(define-module scratch.client
  (extend scratch.common)
  (use dsm.client)
  (export connect-server)
  )
(select-module scratch.client)

(define (connect-server . args)
  (apply (with-module dsm.client connect-server)
         `(,@args
           :eof-handler ,(lambda args #f))))

(provide "scratch/client")