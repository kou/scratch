(define-module number-table-servlet
  (use srfi-2)
  (use gauche.parameter)
  (use scratch.servlet)
  (use scratch.session)
  (use scratch.user.manager.file)
  (use scratch.db.file)
  (use number-table)
  (use number-table-servlet.action)
  (export make-number-table-servlet))
(select-module number-table-servlet)

(define (make-number-table-servlet)
  (make <scratch-servlet>
    :servlet-module-name 'number-table-servlet
    :session-constructor make-session
    :db (make <scratch-db-file>)
    :user-manager (make <user-manager-file>
                    :default-authority 'deny
                    :authority-map '((#t #t)
                                     (#f add-user)))))

(define (make-session)
  (let ((table (make-number-table 3)))
    (shuffle-table! table)
    (let ((sess (make-scratch-session :table table
                                      :count 0)))
      (parameterize ((session sess))
        (update-value!))
      sess)))

(provide "number-table-servlet")