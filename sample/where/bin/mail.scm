#!/usr/bin/env gosh

(use scratch.client.mail)

(define *where-server* "localhost")
(define *where-port* 5979)
(define *where-mount-point* "/where")

(define (main args)
  (scratch-mail-main #`"dsmp://,|*where-server*|:,|*where-port*|"
                     *where-mount-point*
                     (current-input-port)
                     :http-base (get-optional (cdr args) #f)))
