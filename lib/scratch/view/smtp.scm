(define-module scratch.view.smtp
  (extend scratch.common)
  (use srfi-11)
  (use rfc.uri)
  (use gauche.regexp)
  (use gauche.parameter)
  (use file.util)
  (use util.list)
  (use text.tree)
  (use text.html-lite)
  (use esm.gauche)
  (export load-esm-files define-scratch-esm
          default-view)
  )
(select-module scratch.view.smtp)

(define (default-view)
  "error")

(define-macro (define-scratch-esm name filename)
  (let ((args (gensym))
        (src `(esm-result* ,(call-with-input-file filename port->string))))
    `(define (,name . ,args)
       (let-keywords* ,args ((params '()))
         (parameterize ((parameters `(,@params ,@(parameters))))
           ,src)))))

(define-macro (load-esm-files pattern)
  `(begin
     ,@(map (lambda (esm-filename)
              (rxmatch-let (rxmatch #/([^\/]+)\.[^.]+$/ esm-filename)
                  (orig name)
                (let ((proc-name (string->symbol name)))
                  `(begin
                     (export ,proc-name)
                     (define-scratch-esm ,proc-name ,esm-filename)))))
            (call/cc
             (lambda (break)
               (for-each (lambda (path)
                           (let ((files (sys-glob (build-path path pattern))))
                             (if (not (null? files))
                                 (break files))))
                         *load-path*)
               '())))))


(provide "scratch/view/smtp")