#!/usr/bin/env gosh

(use gauche.interactive)
(use dsm.server)
(use scratch.server)
(use login-count-servlet)

(define *login-count-port* 5969)
(define *login-count-mount-point* "/login-count")

(define (main args)
  (let* ((server (make-scratch-server :port *login-count-port*)))
    (add-mount-point! server
                      *login-count-mount-point*
                      (make-login-count-servlet))
    (start-scratch-server server)))
