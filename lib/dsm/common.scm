(define-module dsm.common
  (use text.tree)
  (use srfi-13)
  (use gauche.charconv)
  (use dsm.marshal)
  (export x->dsm-header->string
          dsmp-write dsmp-request dsmp-response
          parse-dsm-header
          version-of encoding-of length-of)
  )
(select-module dsm.common)

(define dsm-version 1)

(define dsm-delimiter ";")

(define-class <dsm-header> ()
  ((version :init-keyword :version :accessor version-of)
   (encoding :init-keyword :encoding :accessor encoding-of)
   (length :init-keyword :length :accessor length-of)
   (command :init-keyword :command :accessor command-of :init-value "get")))

(define (make-dsm-header alist)
  (define (set-dsm-header-slot! header key value)
    (cond ((rxmatch #/^v/ key)
           (slot-set! header 'version (x->number value)))
          ((rxmatch #/^e/ key)
           (slot-set! header 'encoding value))
          ((rxmatch #/^c/ key)
           (slot-set! header 'command value))
          ((rxmatch #/^l/ key)
           (slot-set! header 'length (x->number value)))))
  (let ((dsm-header (make <dsm-header>)))
    (for-each (lambda (elem)
                (set-dsm-header-slot! dsm-header
                                      (x->string (car elem)) (cdr elem)))
              alist)
    dsm-header))

(define (x->dsm-header obj . keywords)
  (make-dsm-header-from-string
   (with-output-to-string (lambda () (write obj)))))

(define (make-dsm-header-from-string str . keywords)
  (make-dsm-header`(("v" . ,dsm-version)
                    ("e" . ,(ces-guess-from-string str "*JP"))
                    ("l" . ,(string-length str))
                    ("c" . ,(get-keyword :command keywords "get")))))

(define (dsm-header->string header)
  (string-append
   (string-join (list #`"v=,(version-of header)"
                      #`"e=,(encoding-of header)"
                      #`"l=,(length-of header)"
                      #`"c=,(command-of header)")
                dsm-delimiter)
   "\n"))

(define (x->dsm-header->string obj . keywords)
  (dsm-header->string (apply x->dsm-header obj keywords)))

(define (parse-dsm-header header)
  (make-dsm-header (map (lambda (elem)
                          (let ((splited-elem (string-split elem "=")))
                            (cons (car splited-elem)
                                  (cadr splited-elem))))
                        (string-split (string-trim-right header)
                                      dsm-delimiter))))

(define (dsmp-core header make-body-proc handle-body-proc)
  (let ((body (make-body-proc header)))
    (handle-body-proc (dsm-header->string (make-dsm-header-from-string body))
                      body)))

(define (dsmp-write header body output)
  (display header output)
  (display body output)
  (flush output))

(define (dsmp-request obj input output handle-object-proc . keywords)
  (let-keywords* keywords ((command "get"))
    (let ((marshaled-obj (marshal obj)))
      (dsmp-write (x->dsm-header->string marshaled-obj :command command)
                  marshaled-obj
                  output)
      (dsmp-core (parse-dsm-header (read-line input))
                 (lambda (dsm-header)
                   (marshal
                    (read-from-string
                     (read-block (length-of dsm-header) input))))
                 (lambda (header body)
                   (handle-object-proc header (unmarshal body)))))))

(define (dsmp-response header input output make-body-proc)
  (dsmp-core (parse-dsm-header header)
             (lambda (dsm-header)
               (let ((body (read-from-string
                            (read-block (length-of dsm-header) input))))
                 (marshal
                  (cond ((string=? "eval" (command-of dsm-header))
                         (apply (unmarshal (car body))
                                (cdr body)))
                        (else (make-body-proc body))))))
             (lambda (header body)
               (dsmp-write header body output))))

(provide "dsm/common")