(define-module scratch.client.mail
  (extend scratch.client)
  (use rfc.822)
  (export scratch-mail-main)
  )
(select-module scratch.client.mail)

(define (scratch-mail-main client mount-point mail . args)
  (let* ((in (cond ((input-port? mail) mail)
                  ((string? mail) (open-input-string mail))
                  (else (error #`",mail must be input-port or string"))))
         (headers (rfc822-header->list in))
         (mail-body (port->string in))
         (id 0)
         (action "")
         (type 'smtp))
    (let ((md (rxmatch #/\[\s*(\S+)\s*\][^\d](\d+)$/
                       (rfc822-header-ref headers "subject" ""))))
      (when md
        (set! id (x->number (md 2)))
        (set! action (md 1)))
      (let* ((dispatch (client mount-point))
             (result (apply dispatch id action type
                            (parse-body (open-input-string mail-body))))
             (header-info (car result))
             (body (cadr result)))
        (send-mail body from to)))))

(define (parse-body body)
  (let ((line (read-line body)))
    (cond ((eof-object? line) '(()))
          ((rxmatch #/^(\S+):\s*$/ line)
           => (lambda (md)
                (do ((next (read-line body) (read-line body))
                     (acc '() (cons next acc)))
                    ((or (eof-object? next)
                         (rxmatch #/^(\S+):/ next))
                     (let ((result `((,(md 1) ,(string-join acc "\n")))))
                       (if (eof-object? next)
                           result
                           (cons result
                                 (parse-body
                                  (open-input-string
                                   (string-join (list next (port->string body))
                                                "\n"))))))))))
          ((rxmatch #/^(\S+):\s*(.+)$/ line)
           => (lambda (md)
                (cons (list (md 1) (md 2))
                      (parse-body body))))
          (else `(("body" ,(string-join (list line (port->string body))
                                        "\n")))))))
                
(provide "scratch/client/mail")