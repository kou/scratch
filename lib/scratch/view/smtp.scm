(define-module scratch.view.smtp
  (extend scratch.view.common)
  (use srfi-19)
  (use text.tree)
  (export rfc2822))
(select-module scratch.view.smtp)

(define-scratch-esm-from-string default-header
  "Content-Type: Text/Plain; charset=<%=(gauche-character-encoding)%>
Content-Transfer-Encoding: 7bit
Subject: Re: [<%=(get-id)%>] <%=(get-action)%>
From: <%=(get-param \"to\")%>
To: <%=(get-param \"from\")%>
Date: <%=(rfc2822)%>
X-Mailer: scratch-client-mail

")
  
(define (default-view)
  (string-append (default-header)
                 "DEFAULT VIEW"))

(define (rfc2822 . time)
  (date->string (time-utc->date (get-optional time (current-time)))
                "~a, ~d ~b ~Y ~X ~z"))

(provide "scratch/view/smtp")
