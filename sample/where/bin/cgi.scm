#!/usr/bin/env gosh

(use dsm.client)
(use scratch.client.cgi)

(define *where-server* "localhost")
(define *where-port* 5979)
(define *where-mount-point* "/where")

(define (main args)
  (scratch-cgi-main (connect-server :host *where-server*
                                    :port *where-port*)
                    *where-mount-point*
                    :debug #t))
