(define-module number-table-servlet
  (use srfi-2)
  (use www.cgi)
  (use gauche.parameter)
  (use scratch.servlet)
  (use scratch.session)
  (use scratch.user.manager.file)
  (use scratch.db.file)
  (use number-table)
  (use number-table-servlet.clear-list)
  (export make-number-table-servlet
          do-move do-deny))
(select-module number-table-servlet)

(define (make-number-table-servlet)
  (make <scratch-servlet>
    :servlet-module (find-module 'number-table-servlet)
    :session-constructor make-session
    :db (make <scratch-db-file>)
    :user-manager (make <user-manager-file>
                    :default-authority 'deny
                    :authority-map '((#t add-user)))))

(define (make-session)
  (let ((table (make-number-table 3)))
    (shuffle-table! table)
    (let ((sess (make-scratch-session :table table
                                      :count 0)))
      (parameterize ((session sess))
        (update-state!))
      sess)))

(define (get-available-ways table)
  (call-with-values
      (lambda () (available-ways table))
    list))

(define (table)
  (get-state 'table))

(define (count)
  (get-state 'count))

(define (game-clear?)
  (get-state 'clear?))

(define (update-state!)
  (set-state! 'clear? (clear? (table)))
  (set-state! 'available-ways (get-available-ways (table))))

(define (do-default)
  (do-main))

(define (do-main)
  (if (game-clear?)
      (begin
        (set-value! (servlet-db) 'clear-list
                    (cons (make-clear-list (get-user) (count))
                          (get-value (servlet-db) 'clear-list '())))
        'clear-list)
      'main))

(define (do-move)
  (let* ((way (get-param "way" :convert string->symbol)))
    (set-state! 'count (+ 1 (count)))
    (move! (table) way)
    (update-state!)
    (do-main)))

(define (do-add-user)
  (or (and-let* ((user (get-param *scratch-user-key*))
                 (user)
                 (password (get-param *scratch-password-key*))
                 (password))
                (if (user-exists? (user-manager) user)
                    (begin
                      (set-session-value!
                       (session) :message #`"user ,|user| already exists.")
                      'add-user)
                    (if (add-user! (user-manager) user password)
                        (begin
                          (set-session-value!
                           (session) :message #`"user ,|user| added.")
                          'login)
                        (begin
                          (set-session-value!
                           (session) :message #`"user ,|user| can't add.")
                          'add-user))))
      'add-user))

(define (do-deny)
  (do-login))

(define (do-login)
  (or (and-let* ((user (get-param *scratch-user-key*))
                 (user)
                 (password (get-param *scratch-password-key*))
                 (password))
                (if (valid-user? user password)
                    'jump-to-main
                    (begin
                      (set-session-value!
                       (session)
                       :message (if (user-exists? user)
                                    "password doesn't match"
                                    #`"user ,|user| doesn't exist"))
                      'login)))
      'login))

(provide "number-table-servlet")