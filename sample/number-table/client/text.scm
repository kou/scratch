#!/usr/bin/env gosh

(use dsm.client)

(define (main args)
  (set! (port-buffering (current-output-port)) :none)
  (let ((client (connect-server :host "localhost" :port 5969)))
    (let ((intep (client "start"))
          (command #f)
          (args '()))
      (do ((res (intep :show) (apply intep command args)))
          ((intep :clear?) (print res (intep :clear?) "clear"))
        (print res)
        (display "> ")
        (set! command (read-line))
        (set! args '())
        (print command)))))

