#!/usr/bin/env gosh

(use dsm.server)
(use number-table)

(define (start-up)
  (let ((table (shuffle-table (make-number-table 3))))
    (lambda (command . args)
      (case (string->symbol command)
        ((show) table)
        ((move) (move (make-keyword (x->string (car args)))
                      table))
        ((clear?) (clear? table))
        (else "bad command")))))

(define (main args)
  (let ((server (make-dsm-server :port 5969)))
    (add-mount-point! server "start" start-up)
    (start-dsm-server server)))
                                           