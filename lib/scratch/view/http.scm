(define-module scratch.view.http
  (extend scratch.common)
  (use srfi-11)
  (use file.util)
  (use util.list)
  (use text.tree)
  (use text.html-lite)
  (use esm.gauche)
  (export load-esm-files define-scratch-esm
          h href form alist->attributes
          default-view)
  )
(select-module scratch.view.http)

(define (h string)
  (with-string-io (x->string string) html-escape))

(define (alist->attributes alist)
  (map (lambda (elem)
         #`",(car elem)=,(h (cadr elem))")
       alist))

(define (href . params)
  (let-values (((id action params) (apply generate-id&action params)))
    (h (string-append (get-param "script-name" "")
                      "?"
                      (string-join (alist->attributes
                                    `(,(list *scratch-id-key* id)
                                      ,(list *scratch-action-key* action)
                                      ,@(slices params 2)))
                                   ";")))))

(define (form . attrs)
  (string-append `("<form action=\""
                   ,(get-param "script-name" "")
                   "\" "
                   ,(string-join (map (lambda (attr)
                                        #`",(car attr)=\",(h (cadr attr))\"")
                                      (slices keywords 2))
                                 " ")
                   ">")))

(define-macro (define-scratch-esm name filename)
  (let ((args (gensym))
        (src `(esm-result* ,(call-with-input-file filename port->string))))
    `(define (,name . ,args)
       (let-keywords* ,args ((params #f))
         (parameterize ((parameters (if params
                                        `(,@params ,@(parameters))
                                        (parameters))))
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

(provide "scratch/view/http")