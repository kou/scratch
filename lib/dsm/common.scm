(define-module dsm.common
  (use text.tree)
  (use srfi-13)
  (use gauche.net)
  (use gauche.charconv)
  (use dsm.marshal)
  (export dsmp-request dsmp-response
          version-of encoding-of length-of id-of
          make-marshal-table-using-socket)
  )
(select-module dsm.common)

(define dsmp-version 1)
(define dsmp-delimiter ";")

; (define-class <remote-object> ()
;   ((eval-proc :init-keyword :eval-proc :accessor eval-proc)
;    (id :accessor id-of)))

; (define-method 

(define-class <dsmp-header> ()
  ((version :init-keyword :version :accessor version-of)
   (encoding :init-keyword :encoding :accessor encoding-of)
   (length :init-keyword :length :accessor length-of)
   (command :init-keyword :command :accessor command-of)))

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
                     ("c" . ,(get-keyword :command keywords)))))

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
;  (p (list "write" header body))
  (display header output)
  (display "\n" output)
  (display body output)
  (flush output))

(define (read-dsmp input . keywords)
  (let-keywords* keywords ((eof-handler (lambda () "Got eof")))
    ;; (p (list "reading..."))
    (let* ((header (read-dsmp-header input eof-handler))
           (body (read-dsmp-body (length-of header) input eof-handler)))
      ;; (p (list "read" (dsmp-header->string header) body))
      (values header body))))
  
(define (read-dsmp-header input eof-handler)
  (let ((header (read-line input)))
    (if (eof-object? header)
        (eof-handler)
        (parse-dsmp-header header))))

(define (read-dsmp-body length input eof-handler)
  (let ((body (read-block length input)))
    (if (eof-object? body)
        (eof-handler)
        (read-from-string body))))

(define (need-remote-eval? obj table)
  (and (reference-object? obj)
       (not (using-same-table? table obj))))

(define (dsmp-request marshaled-obj table in out . keywords)
  (let-keywords* keywords ((command "get")
                           (get-handler (lambda (x) x))
                           (post-handler (lambda (x) x))
                           (eof-handler (lambda ()
                                          (print "Got eof from server"))))
    (define (dsmp-handler)
      (dsmp-write (dsmp-header->string
                   (make-dsmp-header-from-string marshaled-obj
                                                 :command command))
                  marshaled-obj
                  out)
      (receive (header body)
          (read-dsmp in :eof-handler eof-handler)
        (handle-response header body)))

    (define (handle-response header body)
      (let ((obj (handle-dsmp-body (command-of header)
                                   body table
                                   in out
                                   :get-handler get-handler
                                   :response-handler response-handler
                                   :post-handler post-handler)))
        (cond ((string=? "eval" (command-of header))
               (let ((marshaled-obj (marshal table obj)))
                 (dsmp-request marshaled-obj table
                               in out
                               :command "response"
                               :get-handler get-handler
                               :post-handler post-handler)))
              (else obj))))
    
    (define (response-handler obj)
      (if (need-remote-eval? obj table)
          (lambda arg
            (eval-in-remote obj arg table in out
                            :get-handler get-handler
                            :post-handler post-handler))
          (post-handler obj)))

    (dsmp-handler)))

(define (eval-in-remote obj arg table in out . keywords)
  (apply dsmp-request
         (marshal table (cons obj arg))
         table in out
         :command "eval"
         keywords))

(define (handle-dsmp-body command body table in out . keywords)
  (let-keywords* keywords ((response-handler (lambda (x) x))
                           (get-handler (lambda (x) x))
                           (post-handler (lambda (x) x)))
    (cond ((string=? "eval" command)
           (apply (unmarshal table (car body))
                  (map (lambda (elem)
                         (let ((obj (unmarshal table elem)))
                           (if (need-remote-eval? obj table)
                               (lambda arg
                                 (eval-in-remote obj arg table
                                                 in out
                                                 :post-handler post-handler))
                               obj)))
                       (cdr body))))
          ((string=? "response" command) (response-handler body))
          ((string=? "get" command) (get-handler body))
          (else (error "unknown command" command)))))

(define (dsmp-response table input output . keywords)
  (let-keywords* keywords ((eof-handler (lambda ()
                                          (print "Got eof from client"))))
    (receive (header body)
        (read-dsmp input :eof-handler eof-handler)
      (let ((marshalized-body (marshal
                               table
                               (apply handle-dsmp-body
                                      (command-of header)
                                      body table
                                      input output
                                      keywords))))
        (dsmp-write (dsmp-header->string
                     (make-dsmp-header-from-string marshalized-body
                                                   :command "response"))
                    marshalized-body
                    output)))))

(provide "dsm/common")