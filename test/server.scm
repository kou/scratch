#!/usr/bin/env gosh

(use srfi-13)
(use srfi-37)
(use scratch.server)

(define (main args)
  (define default-host "localhost")
  (define default-port 59102)
  (define (usage)
    (print
     #`"\t-h, --host=HOST\t\tUse the given HOST. (default ,|default-host|)")
    (print
     #`"\t-p, --port=PORT\t\tUse the given PORT. (default ,|default-port|)")
    (print "\t-h, --help\t\tDisplay this help.")
    (exit 0))
  (define options
    (list (option '(#\h "host") #f #t
                  (lambda (option name arg host port . others)
                    (values (if (string? arg) arg host)
                            port)))
          (option '(#\p "port") #f #t
                  (lambda (option name arg host port . others)
                    (values host
                            (if (string? arg) arg port))))
          (option '(#\h "help") #f #f
                  (lambda (option name arg . others)
                    (usage)))))
   (receive (host port)
     (args-fold (cdr args)
       options
       (lambda (option name arg . seeds)         ; unrecognized
         (print "Unrecognized option:" name)
         (usage))
       (lambda (operand host part) ; operand
         (values host port))
       default-host
       default-port
       )
     (start-server! (make-server :host host :port port))
   0)
