(define-module scratch.view.http
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
  (export use-cookie-only default-action
          load-esm-files define-scratch-esm
          input
          h hd u ue
          href form alist->attributes
          user-name-input password-input
          default-view)
  )
(select-module scratch.view.http)

(define use-cookie-only (make-parameter #t))
(define default-action (make-parameter #f))

(define (default-view)
  (html:html
   (html:head
    (html:title "DEFAULT VIEW"))
   (html:body
    (html:h1 "DEFUALT VIEW")
    (html:p "This is default view. You must be overwrite this."))))

(define (h str)
  (with-string-io (x->string str) html-escape))

(define (hd str)
  (regexp-replace-all #/&(.*?)\;/ str
    (lambda (md)
      (rxmatch-case (md 1)
        (#/^amp$/i (#f) "&")
        (#/^quot$/i (#f) "\"")
        (#/^gt$/i (#f) ">")
        (#/^lt$/i (#f) "<")
        (#/^#0*(\d+)$/i (#f int)
          (ucs->char (string->number int)))
        (#/^#x([0-9a-f]+)$/i (#f hex)
          (ucs->char (string->number hex 16)))
        (else #`"&,(md 1);")))))
  
(define u uri-encode-string)
(define ud uri-decode-string)

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
                       `(,@(if use-cookie-only
                               '()
                               (list (list *scratch-id-key* id)))
                         ,@(if (or (equal? action *scratch-default-action-name*)
                                   (equal? action (default-action)))
                               '()
                               (list (list *scratch-action-key* action)))
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
                    ,@(if use-cookie-only
                          '()
                          (list (input :type 'hidden
                                       :name *scratch-id-key*
                                       :value id)))
                    ,@(if (or (equal? action *scratch-default-action-name*)
                              (equal? action (default-action)))
                          '()
                          (list (input :type 'hidden
                                       :name *scratch-action-key*
                                       :value action)))))))

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