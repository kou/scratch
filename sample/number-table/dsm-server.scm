#!/usr/bin/env gosh

(use dsm.server)
(use number-table)

(define (main args)
  (let* ((server (make-dsm-server :port 5969))
         (marshal-table (with-module dsm.server (marshal-table-of server)))
         (id-get (with-module dsm.marshal id-get))
         (id-ref (with-module dsm.marshal id-ref)))

    (define (start-up)
      (let ((table (make-number-table 3))
            (count 0))
        (define (dispatch command . args)
          (case (string->symbol (x->string command))
            ((show) table)
            ((move)
             (inc! count)
             (move! (make-keyword (x->string (car args)))
                    table))
            ((clear?) (clear? table))
            ((available-ways) (call-with-values
                                  (lambda () (available-ways table))
                                list))
            ((count) count)
            ((session-info) (id-get marshal-table dispatch))
            ((restore) (id-ref marshal-table (car args)))
            (else "bad command")))

        (shuffle-table! table)
        dispatch))

    (add-mount-point! server "/start" start-up)
    (start-dsm-server server)))
