(define-module scratch.servlet
  (use dsm.marshal)
  (use scratch.common)
  (use scratch.session)
  (export <scratch-servlet> dispatch
          get-param))
(select-module scratch.servlet)

(define-class <scratch-servlet> ()
  ((session-constructor :accessor session-constructor-of
                        :init-keyword :session-constructor)
   (module :accessor module-of :init-keyword :module)
   (session-table :accessor session-table-of
                  :init-form (make-marshal-table))))

(define get-param get-param) ; from scratch.common

(define (get-module module-name)
  (or (find-module module-name)
      (begin
        (load (module-name->path module-name))
        (find-module module-name))))

(define-method dispatch ((self <scratch-servlet>) id action type . args)
  (let* ((session (if id
                      (or (id-ref (session-table-of self) id #f)
                          ((session-constructor-of self)))
                      ((session-constructor-of self))))
         (action-table (module-table (module-of self)))
         (response-module (get-module
                           (string->symbol
                            #`",(module-name (module-of self)).,|type|")))
         (response-table (module-table response-module))
         (dispatch-key (string->symbol #`"do-,|action|")))
    (set! (id-of session)
          (id-get (session-table-of self) session))
    (if (hash-table-exists? action-table dispatch-key)
        (eval `(,dispatch-key ,session ',args) (module-of self)))
    (set-response-value! session *scratch-id-key* (id-of session))
    (let* ((response-key (or (response-key-of session)
                             'default))
           (result (eval `(,response-key :session ,session
                                         :params ',args)
                         response-module)))
      (list (response-info session)
            result))))
;       (if (hash-table-exists? response-table response-key)
;           (eval `(,response-key ,session) response-module)))))

(provide "scratch/servlet")