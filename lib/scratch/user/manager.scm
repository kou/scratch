(define-module scratch.user.manager
  (extend scratch.common)
  (use util.digest)
  (use file.util)
  (use scratch.db)
  (export check-login-do
          store restore
          add-user! remove-user! user-exists?
          valid-user?))
(select-module scratch.user.manager)

(autoload rfc.md5 <md5>)
(autoload rfc.sha1 <sha1>)

(define-class <user-manager> ()
  ((working-directory :accessor working-directory-of
                      :init-keyword :working-directory
                      :init-value "./user")
   (digest-type :accessor digest-type-of
                :init-keyword :digest-type
                :init-form <md5>)
   (default-authority :accessor default-authority-of
                      :init-keyword :default-authority
                      :init-value 'allow)
   (authority-map :accessor authority-map-of
                  :init-keyword :authority-map
                  :init-value '())))

(define-method initialize ((self <user-manager>) args)
  (next-method)
  (make-directory* (working-directory-of self))
  (restore self))

(define-method check-login-do ((self <user-manager>)
                               user password request-action action-handler)
  (if (valid-user? self user password)
      (login! user))
  (action-handler (if (allow? self user request-action)
                      request-action
                      *scratch-deny-action-name*)))

(define (allow? manager user action)
  (or (login? user)
      (let ((auth-info (or (assoc user (authority-map-of manager))
                           (assoc #t (authority-map-of manager)))))
        (if auth-info
            (and (memq action (cdr auth-info))
                 (not (eq? 'allow (default-authority-of manager))))
            (eq? 'allow (default-authority-of manager))))))

(define-method store ((self <user-manager>))
  (error "not implemented"))

(define-method restore ((self <user-manager>))
  (error "not implemented"))

(define-method pass-phrase ((self <user-manager>) user)
  (error "not implemented"))

(define-method add-user! ((self <user-manager>) uesr password . keywrods)
  (error "not implemented"))

(define-method remove-user! ((self <user-manager>) uesr . keywords)
  (error "not implemented"))

(define-method user-exists? ((self <user-manager>) user)
  (error "not implemented"))

(define-method valid-user? ((self <user-manager>) user password)
  (and (string? user)
       (string? password)
       (valid-password? (digest-type-of self)
                        (pass-phrase self user)
                        password)))

(define (valid-password? digest-type required-pass-phrase password)
  (and (string? required-pass-phrase)
       (string? password)
       (string=? required-pass-phrase
                 (compute-pass-phrase digest-type password))))

(define (compute-pass-phrase digest-type password)
  (digest-hexify (digest-string digest-type password)))

(provide "scratch/user/manager")