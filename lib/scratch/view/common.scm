(define-module scratch.view.common
  (extend scratch.common)
  (use file.util)
  (use esm.gauche)
  (export load-esm-files define-scratch-esm
          define-scratch-esm-from-string
          default-view)
  )
(select-module scratch.view.common)

(define (default-view)
  "DEFAULT VIEW")

(define-macro (define-scratch-esm name filename)
  `(define-scratch-esm-from-string
     ,name
     (esm-result* ,(call-with-input-file filename port->string))))

(define-macro (define-scratch-esm-from-string name src)
  (let ((args (gensym)))
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

(provide "scratch/view/common")
