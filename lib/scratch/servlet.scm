(define-module scratch.servlet
  (extend scratch.common)
  (use srfi-2)
  (use gauche.parameter)
  (use gauche.collection)
  (use gauche.threads)
  (use file.util)
  (use text.gettext)
  (use msm.marshal)
  (use dsm.server)
  (use scratch.session)
  (use scratch.user.manager)
  (use scratch.db)
  (require "scratch/view/http") ;; for thread, parameter and load/require
  (export <scratch-servlet> dispatch db-of))
(select-module scratch.servlet)

(autoload scratch.user.manager.null <user-manager-null>)
(autoload scratch.db.null <scratch-db-null>)

(define-class <scratch-servlet> ()
  ((session-constructor :accessor session-constructor-of
                        :init-keyword :session-constructor
                        :init-form make-scratch-session)
   (servlet-module-name :accessor servlet-module-name-of
                        :init-keyword :servlet-module-name)
   (session-table :accessor session-table-of
                  :init-form (make-marshal-table))
   (session-store-filename :accessor session-store-filename-of
                           :init-form
                           (build-path *scratch-default-working-directory*
                                       "session-db.scm"))
   (user-manager :accessor user-manager-of
                 :init-keyword :user-manager
                 :init-form (make <user-manager-null>))
   (db :accessor db-of :init-keyword :db
       :init-form (make <scratch-db-null>))
   (domain :accessor domain-of :init-keyword :domain
           :init-value '("scratch"))
   (gettext-dirs :accessor gettext-dirs-of :init-keyword :gettext-dirs
                 :init-value #f)
   (default-languages :accessor default-languages-of
                      :init-keyword :default-languages :init-value '())
   ))

(define-method initialize ((self <scratch-servlet>) args)
  (next-method)
  (make-directory* (sys-dirname (session-store-filename-of self)))
  (restore self))

(define (get-module module-name)
  (or (find-module module-name)
      (begin
        ;; (p module-name)
        (require-in-root-thread (module-name->path module-name)
                                (current-module))
        (do () ((required-in-root-thread?))
          (thread-sleep! 0.5))
        (find-module module-name))))

(define-method dispatch ((self <scratch-servlet>) id action type . args)
  (let ((sess (get-session self id)))
    (register-session! self sess)
    (parameterize ((session sess)
                   (parameters args)
                   (user-manager (user-manager-of self))
                   (servlet-db (db-of self)))
      (let ((langs (get-param "language" (default-languages-of self)
                              :list #t)))
        (parameterize ((languages langs)
                       (app-gettext (if (app-gettext)
                                      (begin
                                        ((app-gettext) 'set-locale! langs)
                                        (app-gettext))
                                      (make-gettext (domain-of self)
                                                    langs
                                                    (gettext-dirs-of self)))))
          (clear! (session))
          (begin0
              (make-response self
                             (check-login-do (user-manager)
                                (or (get-param *scratch-user-key* #f)
                                    (get-user))
                                (get-param *scratch-password-key* #f)
                                action
                                (cut do-action self type <>)))
            (store self)))))))

(define (get-valid-session table id)
  (and (id-exists? table id)
       (let ((session (id-ref table id)))
         (if (valid-session? session)
           session
           (begin
             (id-delete! table id)
             #f)))))

(define (valid-session? session)
  (and session
       (scratch-session? session)
       (valid? session)))

(define (get-session servlet id)
  (or (and-let* ((id)
                 (session (get-valid-session (session-table-of servlet) id)))
                session)
      ((session-constructor-of servlet))))

(define (register-session! servlet session)
  (let ((id (id-get (session-table-of servlet) session)))
    (set-id! session id)))

(define-method store ((self <scratch-servlet>))
  (store (db-of self))
  (call-with-output-file (session-store-filename-of self)
    (cut store-session self <>)))

(define-method restore ((self <scratch-servlet>))
  (restore (db-of self))
  (if (file-exists? (session-store-filename-of self))
      (call-with-input-file (session-store-filename-of self)
        (cut restore-session self <>))))

(define (store-session servlet out)
  (let ((table (session-table-of servlet)))
    (write (filter (lambda (elem)
                     (and (valid-session? (cdr elem))
                          (marshallable? (car elem))
                          (marshallable? (cdr elem))))
                   (marshal-table->alist table))
           out)))

(define (restore-session servlet in)
  (let ((data (read in)))
    (set! (session-table-of servlet)
          (alist->marshal-table (if (eof-object? data)
                                  '()
                                  data)))))

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
