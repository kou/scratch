#!/usr/bin/env gosh

(use test.unit)
(use scratch.server)
(use scratch.servlet)

(define-class <stub-servlet> ()
  ((value :accessor value-of :init-keyword :value)))

(define-method dispatch ((self <stub-servlet>) id action type . args)
  (value-of self))

(define (make-dsmp-scratch-server port)
  (make-scratch-server #`"dsmp://:,|port|"))

(let ((server #f))
  (define-test-case "scratch server test"
    (setup
     (lambda () (set! server (make-dsmp-scratch-server 7890))))
    (teardown
     (lambda ()
       (scratch-server-stop! server)
       (set! server #f)))
    ("mount-point test"
     (assert-each-list-elem (lambda (key value)
                              (add-mount-point! server key
                                                (make <stub-servlet>
                                                  :value value))
                              (list value
                                    ((get-by-mount-point server key) #f #f #f)))
                            :lists `(("/integer" 1)
                                     ("/string" "string")
                                     ("/list" (1 #t #()))
                                     ("/procedure" ,(lambda () #f))
                                     )))))