(define-module scratch.client.cgi
  (extend scratch.client)
  (use www.cgi)
  (use gauche.charconv)
  (use gauche.regexp)
  (use srfi-1)
  (use rfc.cookie)
  (use text.tree)
  (use text.html-lite)
  (export scratch-cgi-main)
  )
(select-module scratch.client.cgi)

(define (add-meta-info params)
  (fold (lambda (key-info prev)
          (let* ((search-key (car key-info))
                 (regist-key (cadr key-info))
                 (value (with-module www.cgi
                          (if (pair? search-key)
                            (do ((keys search-key (cdr keys))
                                 (result #f
                                         (if (pair? (car keys))
                                           ((cadar keys) (get-meta (caar keys)))
                                           (get-meta (car keys)))))
                                ((or (null? keys)
                                     result)
                                 result)
                              ;; do nothing
                              )
                            (get-meta search-key)))))
            (if value
                (cons (list regist-key value)
                      prev)
                prev)))
        params
        `(((("REQUEST_URI" ,(lambda (value)
                              (and value
                                   (#/\?/ value)
                                   ((#/\?/ value) 'before))))
            "SCRIPT_NAME") "script-name")
          ("HTTP_HOST" "host-name"))))

(define (scratch-cgi-main client mount-point . args)
  (let-keywords* args ((error-proc (cut scratch-error-proc <>
                                        (get-keyword :debug args #f))))
    (cgi-main
     (lambda (params)
       (let* ((params (map (lambda (elem)
                             (map (cut ces-convert <> "*JP")
                                  elem))
                           params))
              (dispatch (client mount-point))
              (id (cgi-get-parameter (x->string *scratch-id-key*)
                                     params
                                     :convert string->number))
              (action (cgi-get-parameter (x->string *scratch-action-key*)
                                         params
                                         :convert string->symbol
                                         :default ""))
              (type 'http)
              (result (apply dispatch id action type (add-meta-info params)))
              (header-info (car result))
              (body (tree->string (cadr result)))
              (cookies (construct-cookie-string
                        `((,(x->string *scratch-id-key*)
                           ,(x->string
                             (get-keyword *scratch-id-key* header-info id))))))
              (content-type
               (get-keyword :content-type header-info
                            (format "text/html; charset=~a"
                                    (normalize-charset
                                     (or (ces-guess-from-string body "*JP")
                                         (gauche-character-encoding)))))))
         `(,(cond ((get-keyword :location header-info #f)
                   => (cut cgi-header
                           :cookies cookies
                           :location <>))
                  (else
                   `(,(cgi-header :cookies cookies
                                  :Content-Type content-type
                                  :Content-Length
                                  (string-size body))
                     ,body))))))
     :merge-cookies #t
     :on-error error-proc)))

(define (scratch-error-proc e debug)
  `(,(cgi-header)
    ,(html-doctype)
    ,(html:html
      (html:head (html:title "Error"))
      (html:body (html:h1 "Error")
                 (html:p (html-escape-string (slot-ref e 'message)))
                 (if debug
                     (html:pre (html-escape-string
                                (call-with-output-string
                                 (cut with-error-to-port <>
                                      (cut report-error e)))))
                     '())
                 (html:hr)
                 (html:p (html-escape-string
                          (format "Using scratch version is ~a"
                                  *scratch-version*)))
                 ))))

(define (normalize-charset charset)
  (rxmatch-case charset
    (#/eucjp/i (orig) "EUC-JP")
    (else charset)))

(provide "scratch/client/cgi")
