#!/usr/bin/env gosh

(use test.unit)
(use scratch.server)

(define-macro (assert-each-by-alist prepare-proc . keywords)
  (let-keywords* keywords ((assert-proc assert-equal)
                           (alist '()))
    `(assert-each ,assert-proc
                  ,alist
                  :prepare (lambda (item)
                             (,prepare-proc (car item) (cdr item))))))

(let ((server #f))
  (define-test-case "scratch server test"
    (setup
     (lambda () (set! server (make-scratch-server :port 7890))))
    (teardown
     (lambda ()
       (stop-scratch-server server)
       (set! server #f)))
    ("mount-point test"
     (assert-each-by-alist (lambda (key value)
                             (add-mount-point! server key value)
                             (list value
                                   (get-by-mount-point server key)))
                           :alist `(("/integer" . 1)
                                    ("/string" . "string")
                                    ("/list" . (1 #t #()))
                                    ("/procedure" . ,(lambda () #f))
                                    )))
    ("store/restore test"
     (let ((test-data (list 1
                            "string"
                            '()
                            (lambda () #f)
                            )))
       (assert-each assert-equal test-data
                    :prepare (lambda (item)
                               (list item
                                     (restore server
                                              (store server item)))))
       (let ((store (get-by-mount-point server
                                        (with-module scratch.common
                                          *scratch-store-mount-point*)))
             (restore (get-by-mount-point server
                                          (with-module scratch.common
                                            *scratch-restore-mount-point*))))
         (assert-each assert-equal test-data
                      :prepare (lambda (item)
                                 (list item (restore (store item))))))))
    ))