(define-module scratch.db.file
  (extend scratch.db)
  (use util.list)
  (use file.util)
  (export <scratch-db-file>))
(select-module scratch.db.file)

(define-method make-db ()
  (make-hash-table 'equal?))

(define-method make-db ((null <null>))
  (make-db))

(define-method make-db ((alist <pair>))
  (alist->hash-table alist 'equal?))

(define-class <scratch-db-file> (<scratch-db>)
  ((filename :accessor filename-of
             :init-keyword :filename
             :init-form
             (build-path *scratch-default-working-directory*
                         "servlet-db.scm"))
   (db :accessor db-of
       :init-thunk make-db)))

(define-method initialize ((self <scratch-db-file>) args)
  (next-method)
  (make-directory* (sys-dirname (filename-of self))))

(define-method store ((self <scratch-db-file>))
  (call-with-output-file (filename-of self)
    (lambda (out)
      (write (hash-table->alist (db-of self)) out))))

(define-method restore ((self <scratch-db-file>))
  (if (file-exists? (filename-of self))
      (set! (db-of self)
            (call-with-input-file (filename-of self)
              (lambda (in)
                (let ((alist (read in)))
                  (if (eof-object? alist)
                      (make-db)
                      (make-db alist))))))))

(define-method get-value ((self <scratch-db-file>) key . default)
  (hash-table-get (db-of self) key (get-optional default #f)))
  
(define-method set-value! ((self <scratch-db-file>) key value)
  (hash-table-put! (db-of self) key value))
  
(define-method remove-value! ((self <scratch-db-file>) key)
  (hash-table-delete! (db-of self) key))

(define-method value-exists? ((self <scratch-db-file>) key)
  (hash-table-exists? (db-of self) key))

(provide "scratch/db/file")