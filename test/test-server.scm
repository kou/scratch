#!/usr/bin/env gosh

(use test.unit)
(use dsm.server)

(let ((server #f))
  (define-test-case "dsm server test"
    (setup
     (lambda () (set! server (make-dsm-server :port 11111))))
    (teardown
     (lambda () (stop-dsm-server server)))
    ("mount test"
     (assert-each assert-equal
                  `(("integer" . 1)
                    ("string" . "str")
                    ("symbol" . sym)
                    ("list" . ())
                    ("vector" . #())
                    ("procedure" . ,(lambda () #f))
                    )
                  :prepare (lambda (item)
                             (let ((key (car item))
                                   (value (cdr item)))
                               (add-mount-point! server key value)
                               (list value
                                     (get-by-mount-point server key)))))
     )
    ))