#!/usr/bin/env gosh

(use dsm.server)
(use number-table)

(define (main args)
  (let ((server (make-dsm-server :port 5969)))
    (add-mount-point! server "start"
                      (lambda (command . args)
                        (let ((table (shuffle-table (make-number-table 3))))
                          (define (move-table way)
                            (move way table)
                            move-table)
                          
                          (case command
                            ((:show) table)
                            ((:move) (move (car args) table))
                            ((:clear?) (clear? table))
                            (else "bad command")))))
    (start-dsm-server server)))
                                           