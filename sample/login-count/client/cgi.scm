#!/usr/bin/env gosh

(use dsm.client)
(use scratch.client.cgi)

(define *login-count-server* "localhost")
(define *login-count-port* 5969)
(define *login-count-mount-point* "/login-count")

(define (main args)
  (scratch-cgi-main (connect-server :host *login-count-server*
                                    :port *login-count-port*)
                    *login-count-mount-point*
                    :debug #t))
