(define-module scratch.common
  (use www.cgi)
  (export *scratch-id-key*
          *scratch-action-key*
          get-param)
  )
(select-module scratch.common)

(define *scratch-id-key* :__scratch_id__)
(define *scratch-action-key* :__scratch_action__)

(define (get-param keyword params . options)
  (apply cgi-get-parameter (x->string keyword) params
         :list #f
         (if (or (null? options)
                 (not (null? (cdr options))))
             options
             (list :default (car options)))))

(provide "scratch/common")
