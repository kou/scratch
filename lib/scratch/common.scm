(define-module scratch.common
  (extend scratch.scratch)
  (use www.cgi)
  (use srfi-1)
  (use srfi-11)
  (use gauche.parameter)
  (use gauche.sequence)
  (export *scratch-id-key* *scratch-user-key*
          *scratch-password-key* *scratch-action-key*
          session parameters user-manager servlet-db
          get-param get-action get-user
          get-servlet-value set-servlet-value!
          delete-servlet-value! servlet-value-exists?
          get-value set-value! delete-value! value-exists?
          get-cycle-value set-cycle-value!
          delete-cycle-value! cycle-value-exists?
          get-response-value set-response-value!
          delete-response-value! response-value-exists?
          get-id set-id!
          generate-id&action
          login! logout! login?
          valid-user? user-exists?)
  )
(select-module scratch.common)

(define *scratch-id-key* :__scratch_id__)
(define *scratch-user-key* :__scratch_user__)
(define *scratch-password-key* :__scratch_password__)
(define *scratch-action-key* :__scratch_action__)

(define *scratch-default-action-name* 'default)
(define *scratch-default-view-name* 'default-view)
(define *scratch-deny-action-name* 'deny)
(define *scratch-new-session-id* 0)

(define *scratch-default-working-directory* ".state")

(define session (make-parameter #f))
(define parameters (make-parameter '()))
(define user-manager (make-parameter #f))
(define servlet-db (make-parameter #f))

(define (get-param keyword . options)
  (apply cgi-get-parameter (x->string keyword) (parameters)
         `(,@(if (and (not (null? options))
                      (not (keyword? (car options))))
                 (cons :default options)
                 options)
           :list #f)))

(define-method get-servlet-value (key . fall-back)
  (apply get-value (servlet-db) key fall-back))
(define-method set-servlet-value! (key value)
  (set-value! (servlet-db) key value))
(define-method delete-servlet-value! (key)
  (delete-value! (servlet-db) key))
(define-method servlet-value-exists? (key)
  (value-exists? (servlet-db) key))

(define-method get-value (key . fall-back)
  (apply get-value (session) key fall-back))
(define-method set-value! (key value)
  (set-value! (session) key value))
(define-method delete-value! (key)
  (delete-value! (session) key))
(define-method value-exists? (key)
  (value-exists? (session) key))

(define-method get-cycle-value (key . fall-back)
  (apply get-cycle-value (session) key fall-back))
(define-method set-cycle-value! (key value)
  (set-cycle-value! (session) key value))
(define-method delete-cycle-value! (key)
  (delete-cycle-value! (session) key))
(define-method cycle-value-exists? (key)
  (cycle-value-exists? (session) key))

(define-method get-response-value (key . fall-back)
  (apply get-response-value (session) key fall-back))
(define-method set-response-value! (key value)
  (set-response-value! (session) key value))
(define-method delete-response-value! (key)
  (delete-response-value! (session) key))
(define-method response-value-exists? (key)
  (response-value-exists? (session) key))

(define-method get-id ()
  (get-id (session)))
(define-method get-id (session)
  (get-value session *scratch-id-key* #f))
(define-method set-id! ()
  (set-id! (session)))
(define-method set-id! (session)
  (set-id! session (id-of session)))
(define-method set-id! (session id)
  (set-value! session *scratch-id-key* id))

(define (get-action)
  (get-param *scratch-action-key*))

(define (get-user)
  (get-value *scratch-user-key* #f))

(define (generate-id&action . args)
  (let ((default-id (get-id)))
    (let loop ((id default-id)
               (action (or (get-action)
                           *scratch-default-action-name*))
               (args args))
      (cond ((null? args) (values id action args))
            ((memq (car args) '(:new-session :action))
             => (lambda (keywords)
                  (case (car keywords)
                    ((:new-session) (loop (if (cadr args)
                                              *scratch-new-session-id*
                                              default-id)
                                          action
                                          (cddr args)))
                    ((:action) (loop id (cadr args) (cddr args))))))
            (else (let ((ind (find-index (cut memq <> '(:new-session :action))
                                         args)))
                    (if ind
                        (loop id action
                              (let-values (((before after) (split-at args ind)))
                                `(,@(take after 2)
                                  ,@before
                                  ,@(cddr after))))
                        (values id action args))))))))

(define (login! user)
  (set-value! *scratch-user-key* user))
(define (logout!)
  (set-value! *scratch-user-key* #f))
(define (login? user)
  (let ((current-user (get-user)))
    (and (string? user)
         (string? current-user)
         (string=? user current-user))))

(define-method valid-user? (user password)
  (valid-user? (user-manager) user password))
(define-method user-exists? (user)
  (user-exists? (user-manager) user))

(provide "scratch/common")
