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
          user-name-input password-input
          default-view)
  )
(select-module scratch.view.http)

(define (default-view)
  (html:html
   (html:head
    (html:title "DEFAULT VIEW"))
   (html:body
    (html:h1 "DEFUALT VIEW")
    (html:p "This is default view. You must be overwrite this."))))

(define (h string)
  (with-string-io (x->string string) html-escape))

(define (alist->params alist)
  (map (lambda (elem)
         #`",(car elem)=,(h (cadr elem))")
       alist))

(define (params->string params)
  (string-join params ";"))

(define (alist->params-string alist)
  (params->string (alist->params alist)))

(define (alist->attributes alist)
  (map (lambda (elem)
         #`",(car elem)=\",(h (cadr elem))\"")
       alist))

(define (attributes->string attrs)
  (string-join attrs " "))

(define (alist->attributes-string alist)
  (attributes->string (alist->attributes alist)))

(define (href . params)
  (let-values (((id action params) (apply generate-id&action params)))
    (h (string-append (get-param "script-name" "")
                      "?"
                      (alist->params-string
                       `(,(list *scratch-id-key* id)
                         ,(list *scratch-action-key* action)
                         ,@(slices params 2)))))))

(define (input . keywords)
  (tree->string `("<input "
                  ,(alist->attributes-string (slices keywords 2))
                  " />")))

(define (form . attrs)
  (let-values (((id action attrs) (apply generate-id&action attrs)))
    (tree->string `("<form action=\""
                    ,(get-param "script-name" "")
                    "\" "
                    ,(alist->attributes-string (slices attrs 2))
                    ">\n"
                    ,(input :type 'hidden
                            :name *scratch-id-key*
                            :value id)
                    ,(input :type 'hidden
                            :name *scratch-action-key*
                            :value action)))))

(define (user-name-input . attrs)
  (apply input
         :type 'text
         :name *scratch-user-key*
         :value (or (get-param *scratch-user-key* #f)
                    (get-user)
                    "")
         attrs))

(define (password-input . attrs)
  (apply input
         :type 'password
         :name *scratch-password-key*
         attrs))

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

(provide "scratch/view/http")