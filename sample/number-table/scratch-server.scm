#!/usr/bin/env gosh

(use scratch.server)
(use number-table-servlet)

(define (main args)
  (let* ((server (make-dsm-server :port 5969)))
    (add-mount-point! server
                      :mount-point "/number-table"
                      :servlet (make-number-table-servlet))
    (start-dsm-server server)))
