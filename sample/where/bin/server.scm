#!/usr/bin/env gosh

(use gauche.interactive)
(use scratch.server)
(use where.servlet)

(define *where-port* 5979)
(define *where-mount-point* "/where")

(define (main args)
  (let* ((server (make-scratch-server #`"dsmp://:,|*where-port*|")))
    (add-mount-point! server
                      *where-mount-point*
                      (make-where-servlet))
    (scratch-server-start! server)
    (scratch-server-join! server)))
