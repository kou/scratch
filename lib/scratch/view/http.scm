(define-module scratch.view.http
  (use file.util)
  (use www.cgi)
  (use util.list)
  (use esm.gauche)
  (use gauche.parameter)
  (use scratch.common)
  (use scratch.session)
  (use text.html-lite)
  (export load-esm-files define-scratch-esm
          session parameters
          get-param get-state
          h href
          parameterize-if-need)
  )
(select-module scratch.view.http)

(define (h string)
  (with-string-io (x->string string) html-escape))

(define session (make-parameter #f))
(define parameters (make-parameter '()))

(define _scratch-get-param get-param)
(define (get-param keyword . fall-back)
  (apply _scratch-get-param keyword (parameters) fall-back))

(define (get-state keyword . fall-back)
  (apply get-value (session) keyword fall-back))
(define (get-id)
  (get-response-value (session) *scratch-id-key*))

(define (href . params)
  (let* ((id (if (eq? :new-session (get-optional params #f))
                 (begin
                   (set! params (cdr params))
                   0)
                 (get-id)))
         (action (if (eq? :action (get-optional params #f))
                     (begin0
                         (cadr params)
                       (set! params (cddr params)))
                     'default)))
    (h (string-append (get-param "script-name" "")
                      "?"
                      (string-join (map (lambda (elem)
                                          #`",(car elem)=,(cadr elem)")
                                        `(,(list *scratch-id-key* id)
                                          ,(list *scratch-action-key* action)
                                          ,@(slices params 2)))
                                   ";")))))

(define-macro (define-scratch-esm name filename)
  (let ((args (gensym))
        (src `(esm-result* ,(call-with-input-file filename port->string))))
    `(define (,name . ,args)
       (let-keywords* ,args ((sess :session (session))
                             (params #f))
         (parameterize ((parameters (if params
                                        `(,@params ,@(parameters))
                                        (parameters)))
                        (session sess))
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