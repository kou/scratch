#!/usr/local/bin/gosh

(use dsm.client)
(use scratch.client.cgi)

(define *number-table-server* "localhost")
(define *number-table-port* 5969)
(define *number-table-mount-point* "/number-table")

(define (main args)
  (scratch-cgi-main (connect-server :host *number-table-server*
                                    :port *number-table-port*)
                    *number-table-mount-point*
                    :debug #t))
