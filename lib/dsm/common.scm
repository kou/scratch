(define-module dsm.common
  (use text.tree)
  (use srfi-13)
  (use gauche.net)
  (use gauche.charconv)
  (use dsm.marshal)
  (export dsmp-request dsmp-response
          version-of encoding-of length-of
          make-marshal-table-using-socket)
  )
(select-module dsm.common)

(define dsmp-version 1)
(define dsmp-delimiter ";")

(define-class <dsmp-header> ()
  ((version :init-keyword :version :accessor version-of)
   (encoding :init-keyword :encoding :accessor encoding-of)
   (length :init-keyword :length :accessor length-of)
   (command :init-keyword :command :accessor command-of :init-value "get")))

(define (get-sock-host&port socket)
  (let* ((md (rxmatch #/(.*):(\d+)$/
                      (sockaddr-name (socket-address socket)))))
    (list (md 1)
          (string->number (md 2)))))

(define (make-marshal-table-using-socket socket)
  (apply make-marshal-table
         (get-sock-host&port socket)))

(define (make-dsmp-header alist)
  (define (set-dsmp-header-slot! header key value)
    (cond ((rxmatch #/^v/ key)
           (slot-set! header 'version (x->number value)))
          ((rxmatch #/^e/ key)
           (slot-set! header 'encoding value))
          ((rxmatch #/^c/ key)
           (slot-set! header 'command value))
          ((rxmatch #/^l/ key)
           (slot-set! header 'length (x->number value)))))

  (let ((dsmp-header (make <dsmp-header>)))
    (for-each (lambda (elem)
                (set-dsmp-header-slot! dsmp-header
                                       (x->string (car elem)) (cdr elem)))
              alist)
    dsmp-header))

(define (x->dsmp-header table obj . keywords)
  (apply make-dsmp-header-from-string (marshal table obj) keywords))

(define (make-dsmp-header-from-string str . keywords)
  (make-dsmp-header`(("v" . ,dsmp-version)
                     ("e" . ,(ces-guess-from-string str "*JP"))
                     ("l" . ,(string-length str))
                     ("c" . ,(get-keyword :command keywords "get")))))

(define (dsmp-header->string header)
  (string-join (list #`"v=,(version-of header)"
                     #`"e=,(encoding-of header)"
                     #`"l=,(length-of header)"
                     #`"c=,(command-of header)")
               dsmp-delimiter))

(define (x->dsmp-header->string table obj . keywords)
  (dsmp-header->string (apply x->dsmp-header table obj keywords)))

(define (parse-dsmp-header str)
  (make-dsmp-header (map (lambda (elem)
                          (let ((splited-elem (string-split elem "=")))
                            (cons (car splited-elem)
                                  (cadr splited-elem))))
                        (string-split (string-trim-right str)
                                      dsmp-delimiter))))

(define (dsmp-write header body output)
  (display header output)
  (display "\n" output)
  (display body output)
  (flush output))

(define (read-dsmp input)
  (let ((header (read-dsmp-header input)))
    (values header
            (read-dsmp-body (length-of header) input))))

(define (read-dsmp-header input)
  (parse-dsmp-header (read-line input)))

(define (read-dsmp-body length input)
  (read-from-string
   (read-block length input)))

(define (dsmp-request marshaled-obj table in out post-handler . keywords)
  (let-keywords* keywords ((command "get"))
    (define (dsmp-handler)
      ;; (p marshaled-obj output)
      (dsmp-write (dsmp-header->string
                   (make-dsmp-header-from-string marshaled-obj
                                                 :command command))
                  marshaled-obj
                  out)
      (receive (header body)
          (read-dsmp in)
        ;; (p (command-of header) body input)
        (response-handler header body)))

    (define (response-handler header body)
      (handle-dsmp-body (command-of header)
                        body table
                        in out
                        get-handler))
    
    (define (get-handler obj)
      (if (reference-object? obj)
          (lambda arg
            (eval-in-remote obj arg table in out get-handler))
          (post-handler obj)))

    (dsmp-handler)))

(define (eval-in-remote obj arg table in out post-handler)
  (dsmp-request (marshal table (cons obj arg))
                table in out post-handler
                :command "eval"))

(define (handle-dsmp-body command body table in out make-body-proc)
  (cond ((string=? "eval" command)
         (apply (unmarshal table (car body))
                (map (lambda (elem)
                       (let ((obj (unmarshal table elem)))
                         (if (and (reference-object? obj)
                                  (not (using-same-table? table obj)))
                             (lambda arg
                               (eval-in-remote obj arg table
                                               in out make-body-proc))
                             obj)))
                     (cdr body))))
        (else (make-body-proc body))))

(define (dsmp-response table input output make-body-proc)
  (receive (header body)
      (read-dsmp input)
    (let ((marshalized-body (marshal
                             table
                             (handle-dsmp-body (command-of header)
                                               body table
                                               input output
                                               make-body-proc))))
      (dsmp-write (dsmp-header->string
                   (make-dsmp-header-from-string marshalized-body))
                  marshalized-body
                  output))))

(provide "dsm/common")