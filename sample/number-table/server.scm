#!/usr/bin/env gosh

(use dsm.server)
(use number-table)

(define (start-up)
  (let ((table (make-number-table 5)))
    (shuffle-table! table)
    (lambda (command . args)
      (case (string->symbol (x->string command))
        ((show) table)
        ((move) (move! (make-keyword (x->string (car args)))
                       table))
        ((clear?) (clear? table))
        (else "bad command")))))

; (define (continue server id)
  

(define (main args)
  (let ((server (make-dsm-server :port 5969)))
    (add-mount-point! server "/start" start-up)
;    (add-mount-point! server "/continue" continue)
    (start-dsm-server server)))
