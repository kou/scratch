#!/usr/bin/env gosh

(use scratch.client.cgi)

(define *number-table-server* "localhost")
(define *number-table-port* 5969)
(define *number-table-mount-point* "/number-table")

(define (main args)
  (scratch-cgi-main #`"dsmp://,|*number-table-server*|:,|*number-table-port*|"
                    *number-table-mount-point*
                    :debug #t))
