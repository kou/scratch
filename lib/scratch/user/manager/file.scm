(define-module scratch.user.manager.file
  (extend scratch.user.manager)
  (use util.list)
  (export <user-manager-file>))
(select-module scratch.user.manager.file)

(define-class <user-manager-file> (<user-manager>)
  ((filename :accessor filename-of
             :init-keyword :filename
             :init-value ".passwd")
   (user-db :accessor user-db-of
            :init-form (make-hash-table 'string=?))))

(define-method store ((self <user-manager-file>))
  (call-with-output-file (filename-of self)
    (lambda (out)
      (write (hash-table->alist (user-db-of self)) out))))

(define-method restore ((self <user-manager-file>))
  (if (file-exists? (filename-of self))
      (set! (user-db-of self)
            (call-with-input-file (filename-of self)
              (lambda (in)
                (alist->hash-table (read in) 'string=?))))))

(define-method add-user! ((self <user-manager-file>) uesr password . keywrods)
  (define (add)
    (let ((can-add? (and (string? user) (string? password))))
      (if can-add?
          (hash-table-put! (user-db-of self)
                           (list user
                                 (compute-pass-phrase (digest-type-of self)
                                                      password))))
      can-add?))
  (let-keywords* keywords ((if-exists #f))
    (if (user-exists? self user)
        (case if-exists
          ((:update) (add))
          ((:error) (error "User already exists."))
          ((#f) #f))
        (add))))

(define-method remove-user! ((self <user-manager-file>) uesr . keywords)
  (define (remove)
    (hash-table-delete! (user-db-of self) user)
    #t)
  (let-keywords* keywords ((if-does-not-exist #f))
    (if (user-exists? self user)
        (remove)
        (case if-does-not-exist
          ((:error) (error "User does not exist."))
          ((#f) #f)))))

(define-method user-exists? ((self <user-manager-file>) user)
  (and (string? user)
       (hash-table-exists? (user-db-of self) user)))

(define-method pass-phrase ((self <user-manager-file>) user)
  (if (and (string? user)
           (hash-table-exists? (user-db-of self) user))
      (cadr (hash-table-get (user-db-of self) user))
      #f))

(provide "scratch/user/manager/file")