(define-module scratch.servlet
  (extend scratch.common)
  (use srfi-2)
  (use marshal)
  (use scratch.session)
  (use scratch.user.manager)
  (use scratch.db)
  (use gauche.parameter)
  (use file.util)
  (export <scratch-servlet> dispatch))
(select-module scratch.servlet)

(autoload scratch.user.manager.null <user-manager-null>)
(autoload scratch.db.null <scratch-db-null>)

(define-class <scratch-servlet> ()
  ((session-constructor :accessor session-constructor-of
                        :init-keyword :session-constructor
                        :init-form (make-scratch-session))
   (servlet-module-name :accessor servlet-module-name-of
                        :init-keyword :servlet-module-name)
   (session-table :accessor session-table-of
                  :init-form (make-marshal-table))
   (user-manager :accessor user-manager-of
                 :init-keyword :user-manager
                 :init-form (make <user-manager-null>))
   (working-directory :accessor working-directory-of
                      :init-keyword :working-directory
                      :init-value ".")
   (db :accessor db-of :init-keyword :db
       :init-form (make <scratch-db-null>))
   ))

(define-method initialize ((self <scratch-servlet>) args)
  (next-method)
  (make-directory* (working-directory-of self))
  (restore (db-of self) (working-directory-of self)))

(define (get-module module-name)
  (or (find-module module-name)
      (begin
        (load (module-name->path module-name))
        (find-module module-name))))

(define-method dispatch ((self <scratch-servlet>) id action type . args)
  (let ((sess (get-session self id)))
    (register-session! self sess)
    (parameterize ((session sess)
                   (parameters args)
                   (user-manager (user-manager-of self))
                   (servlet-db (db-of self)))
      (clear! (session))
      (begin0
          (make-response self
                         (check-login-do (user-manager)
                            (or (get-param *scratch-user-key* #f)
                                (get-user))
                            (get-param *scratch-password-key* #f)
                            action
                            (cut do-action self type <>)))
        (store (db-of self) (working-directory-of self))))))

(define (get-valid-session table id)
  (and (id-exists? table id)
       (let ((session (id-ref table id)))
         (begin0
             (and session (valid? session) session)
           (when (not (valid? session))
             (id-delete! table id))))))
      
(define (get-session servlet id)
  (or (and-let* ((id)
                 (session (get-valid-session (session-table-of servlet) id)))
                session)
      ((session-constructor-of servlet))))

(define (register-session! servlet session)
  (let ((id (id-get (session-table-of servlet) session)))
    (set! (id-of session) id)
    (set-id! session id)))

(define (eval-exported-proc module key default)
  (let ((table (module-table module)))
    (eval `(,(if (hash-table-exists? table key)
                 key
                 default))
          module)))

(define (do-action servlet type action)
  (make-result servlet
               type
               (eval-exported-proc
                (get-action-module servlet)
                (string->symbol #`"do-,|action|")
                (string->symbol #`"do-,|*scratch-default-action-name*|"))))

(define (make-result servlet type view-name)
  (eval-exported-proc (get-response-module servlet type)
                      view-name
                      *scratch-default-view-name*))

(define (get-action-module servlet)
  (get-module
   (string->symbol
    #`",(servlet-module-name-of servlet).action")))

(define (get-response-module servlet type)
  (get-module
   (string->symbol
    #`",(servlet-module-name-of servlet).view.,|type|")))

(define (make-response servlet result)
  (list (response-info-list (session))
        result))

(provide "scratch/servlet")