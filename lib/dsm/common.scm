(define-module dsm.common
  (use text.tree)
  (use srfi-13)
  (use gauche.charconv)
  (use dsm.marshal)
  (export x->dsm-header->string
          dsmp-write dsmp-request dsmp-response
          parse-dsm-header
          get-dsm-object-from-remote
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
  (apply make-dsm-header-from-string (marshal obj) keywords))

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

(define (dsmp-write header body output)
  (display header output)
  (display body output)
  (flush output))

(define (get-dsm-body length input)
  (read-from-string
   (read-block length input)))

(define (dsmp-request marshaled-obj input output handle-object-proc . keywords)
  (let-keywords* keywords ((command "get"))
    (dsmp-write (dsm-header->string
                 (make-dsm-header-from-string marshaled-obj :command command))
                marshaled-obj
                output)
    (let* ((dsm-header (parse-dsm-header (read-line input)))
           (body (get-dsm-body (length-of dsm-header) input)))
      (handle-object-proc body))))

(define (get-dsm-object-from-remote obj in out . options)
  (let-optionals* options ((command "get"))
    (dsmp-request obj in out
                  (lambda (object)
                    (if (referenced-object? object)
                        (lambda arg
                          (p object)
                          (eval-in-remote object arg in out))
                        object))
                  :command command)))

(define (eval-in-remote obj arg in out)
  (get-dsm-object-from-remote (cons obj arg) in out "eval"))

(define (handle-dsmp-body command body make-body-proc)
  (cond ((string=? "eval" command)
         (p body)
         (ct)
         (apply (unmarshal (car body))
                (cdr body)))
        (else (make-body-proc body))))

(define (dsmp-response header input output make-body-proc)
  (let* ((dsm-header (parse-dsm-header header))
         (body (get-dsm-body (length-of dsm-header) input))
         )
    (let (
         (marshalized-body (marshal
                            (handle-dsmp-body (command-of dsm-header)
                                              body
                                              make-body-proc))))
    (dsmp-write (dsm-header->string
                 (make-dsm-header-from-string marshalized-body))
                marshalized-body
                output)))
  )

(provide "dsm/common")