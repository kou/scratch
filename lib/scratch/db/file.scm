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
             :init-value "servlet-db.scm")
   (db :accessor db-of
       :init-thunk make-db)))

(define-method store ((self <scratch-db-file>) base-dir)
  (let ((file (build-path base-dir (filename-of self))))
    (call-with-output-file file
      (lambda (out)
        (write (hash-table->alist (db-of self)) out)))))

(define-method restore ((self <scratch-db-file>) base-dir)
  (let ((file (build-path base-dir
                          (filename-of self))))
    (if (file-exists? file)
        (set! (db-of self)
              (call-with-input-file file
                (lambda (in)
                  (let ((alist (read in)))
                    (if (eof-object? alist)
                        (make-db)
                        (make-db alist)))))))))

(define-method get-value ((self <scratch-db-file>) key . default)
  (hash-table-get (db-of self) key (get-optional default #f)))
  
(define-method set-value! ((self <scratch-db-file>) key value)
  (hash-table-put! (db-of self) key value))
  
(define-method remove-value! ((self <scratch-db-file>) key)
  (hash-table-delete! (db-of self) key))

(define-method value-exists? ((self <scratch-db-file>) key)
  (hash-table-exists? (db-of self) key))

(provide "scratch/db/file")