(define-module scratch.servlet
  (use dsm.marshal)
  (export))
(select-module scratch.servlet)

(define-class <scratch-servlet> ()
  ((session-constructor :accessor session-constructor-of
                        :init-keyword :session-constructor)
   (module :accessor module-of :init-keyword :module)
   (session-table :accessor session-table-of)))

(define-method initialize ((self <scratch-servlet>) args)
  (next-method)
  (set! (session-table-of self) (make-marshal-table)))

(define-method dispatch ((self <scratch-servlet>) id key type . args)
  (let ((session (if id
                     (id-ref (session-table-of self) id)
                     ((session-constructor-of self))))
        (action-table (module-table (module-of self)))
        (response-table (module-table
                         (find-module
                          #`",(module-name (module-of self)).,|type|")))
        (dispatch-key (string-symbol #`"do-,|key|")))
    (if (hash-table-exists? action-table dispatch-key)
        ((hash-table-get action-table dispatch-key) session args))
    (id-get (session-table-of self) session)
    ((hash-table-get response-table (response-key-of session))
     (session->keyword-list session))))

(provide "scratch/servlet")