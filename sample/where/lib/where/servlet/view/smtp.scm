(define-module where.servlet.view.smtp
  (use srfi-19)
  (use gauche.parameter)
  (use sxml.tools)
  (use text.tree)
  (use esm.gauche)
  (use scratch.view.smtp))
(select-module where.servlet.view.smtp)

(load-esm-files "where/servlet/view/smtp/*.esm")

(define (move-links)
  (tree->string
   (map (lambda (where)
          (list "[" where "]"))
        '("left" "center" "right"))))

(define (rfc2822)
  (date->string (time-utc->date (current-time))
                "~a, ~d ~b ~Y ~X ~z"))

(define (default-view)
  (show))

(provide "where/servlet/view/smtp")
