#!/usr/bin/env gosh

(use gauche.interactive)
(use dsm.server)
(use scratch.server)
(use where.servlet)

(define *where-port* 5979)
(define *where-mount-point* "/where")

(define (main args)
  (let* ((server (make-scratch-server :port *where-port*)))
    (add-mount-point! server
                      *where-mount-point*
                      (make-where-servlet))
    (start-scratch-server server)))
