#!/usr/bin/env gosh

(use dsm.client)

(define (display-result res)
  (if (pair? res)
      (for-each print res)
      (print res)))

(define (display-prompt count)
  (display #`",|count|> "))

(define (parse-input input)
  (let ((tokens (string-split input #/\s+/)))
    (values (car tokens)
            (cdr tokens))))

(define (main args)
  (set! (port-buffering (current-output-port)) :none)
  (let ((client (connect-server :host "localhost" :port 5969)))
    (let ((intep ((client "/start")))
          (command #f)
          (args '()))
      (do ((res (intep "show") (apply intep command args)))
          ((intep "clear?") (print "CLEAR!!!!"))
        (display-result res)
        (display-prompt (intep "count"))
        (set!-values (command args) (parse-input (read-line)))
        (print command)))))

