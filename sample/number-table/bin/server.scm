#!/usr/bin/env gosh

(use gauche.interactive)
(use scratch.server)
(use number-table-servlet)

(define *number-table-port* 5969)
(define *number-table-mount-point* "/number-table")

(define (main args)
  (let ((servlet (make-number-table-servlet))
        (server (make-scratch-server #`"dsmp://:,|*number-table-port*|")))
    (add-mount-point! server *number-table-mount-point* servlet)
    (scratch-server-start! server)
    (scratch-server-join! server)))
