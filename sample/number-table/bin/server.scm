#!/usr/bin/env gosh

(use gauche.interactive)
(use dsm.server)
(use scratch.server)
(use number-table-servlet)

(define *number-table-port* 5969)
(define *number-table-mount-point* "/number-table")

(define (main args)
  (let* ((server (make-scratch-server :port *number-table-port*)))
    (add-mount-point! server
                      *number-table-mount-point*
                      (make-number-table-servlet))
    (start-scratch-server server)))