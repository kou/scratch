#!/usr/bin/env gosh

(use test.unit)
(use gauche.process)
(use dsm.client)
(load "test/server-conf")

(let ((server-command "./test/server.scm")
      (server-host "localhost")
      (server-port 59102)
      (process #f))
  (define-test-case "Server test"
    (setup
     (lambda ()
       (set! process (run-process server-command
                                  "--host" server-host
                                  "--port" server-port))))
    (teardown
     (lambda ()
       (process-kill process)))
    ("marshalizable object test"
     (let ((server (connect-server :host server-host
                                   :port server-port)))
       (for-each (lambda (key&value)
                   (assert-equal (cdr key&value) (server (car key&value))))
                 marshalizable-key&value-alist)))
    ))
