#!/usr/bin/env gosh

(use test.unit)
(use dsm.marshal)
(use dsm.common)

(define-assertion (assert-dsm-header header version encoding length command)
  (define (make-message-handler expect type)
    (lambda (actual)
      (format " expected <~s> ~a\n  but was <~s>"
              expect type actual)))
  (let* ((parsed-header (with-module dsm.common (parse-dsm-header header)))
         (header-version (with-module dsm.common (version-of parsed-header)))
         (header-encoding (with-module dsm.common (encoding-of parsed-header)))
         (header-length (with-module dsm.common (length-of parsed-header)))
         (header-command (with-module dsm.common (command-of parsed-header))))
    (assert-equal header-version version
                  (make-message-handler header-version "version"))
    (assert-equal header-encoding encoding
                  (make-message-handler header-encoding "encoding"))
    (assert-equal header-length length
                  (make-message-handler header-length "length"))
    (assert-equal header-command command
                  (make-message-handler header-command "command"))
    ))
    
(define-test-case "dsm common library test"
  ("make-header test"
   (assert-each assert-equal
                `(("v=1;e=UTF-8;l=1;c=get\n" . 1)
                  ("v=1;e=UTF-8;l=5;c=get\n" . "abc")
                  ("v=1;e=UTF-8;l=2;c=get\n" . ())
                  ("v=1;e=UTF-8;l=5;c=get\n" . (1 2))
                  )
                :prepare (lambda (item)
                           (list (car item)
                                 (x->dsm-header->string (cdr item)))))
   )
  ("parse-header test"
   (assert-each assert-dsm-header
                `(("v=1;e=UTF-8;l=1;c=get\n" 1 "UTF-8" 1 "get")
                  ("version=1;encoding=UTF-8;length=1;command=eval\n"
                   1 "UTF-8" 1 "eval")
                  ("v=1.1;e=EUC-JP;l=3;c=get\n" 1.1 "EUC-JP" 3 "get")
                  ))
   )
  ("dsmp-response test"
   (assert-each assert-equal
                `(1 "abc" ,(lambda (x) x))
                :prepare
                (lambda (item)
                  (list (string-append (x->dsm-header->string item)
                                       (marshal item))
                        (let ((in (open-input-string (marshal item)))
                              (out (open-output-string)))
                          (dsmp-response
                           (x->dsm-header->string (marshal item))
                           in
                           out
                           (lambda (body) (unmarshal body)))
                          (get-output-string out)))))
   )
  )