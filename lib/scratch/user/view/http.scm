(define-module scratch.user.view.http
  (extend scratch.common)
  (use scratch.view.http)
  (export login-form)
  )
(select-module scratch.user.view.http)

(define (login-form . attrs)
  (let-values (((id action attrs) (apply generate-id&action attrs)))
    (string-join `(,(apply form attrs)
                   ,@(map (lambda (alist)
                            (string-append
                             "\t<input type=\"hidden\" "
                             (string-join (alist->attributes alist)
                                          " ")
                             " />"))
                          `(((name ,*scratch-id-key*)
                             (value ,id))
                            ((name ,*scratch-action-key*)
                             (value ,action)))
                          ))
                 "\n")))

(provide "scratch/user/view/http")