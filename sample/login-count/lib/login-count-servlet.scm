(define-module login-count-servlet
  (use scratch.servlet)
  (use scratch.session)
  (use scratch.user.manager.file)
  (use scratch.db.file)
  (export make-login-count-servlet))
(select-module login-count-servlet)

(define (make-login-count-servlet)
  (let ((db (make <scratch-db-file>)))
    (set-value! db 'count 0)
    (make <scratch-servlet>
      :servlet-module-name 'login-count-servlet
      :session-constructor (lambda () (make-scratch-session :count 0))
      :db db
      :user-manager (make <user-manager-file>
                      :default-authority 'deny
                      :authority-map '((#t #t)
                                       (#f login))))))

(provide "login-count-servlet")
