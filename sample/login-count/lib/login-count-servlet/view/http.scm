(define-module login-count-servlet.view.http
  (use gauche.parameter)
  (use esm.gauche)
  (use scratch.view.http)
  (export jump-to-main))
(select-module login-count-servlet.view.http)

(load-esm-files "login-count-servlet/view/http/*.esm")

(define (default-view)
  (main))

(define (jump-to-main)
  (set-response-value!
    :location (href :action 'main
		    *scratch-user-key* (get-param *scratch-user-key*)
		    *scratch-password-key* (get-param *scratch-password-key*)))
  "")

(provide "login-count-servlet/view/http")