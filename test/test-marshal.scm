#!/usr/bin/env gosh

(use test.unit)
(use dsm.marshal)

(let ((table #f)
      (host "localhost")
      (port 59100))
  (define-test-case "Marshal test"
    (setup
     (lambda () (set! table (make-marshal-table host port))))
    ("can marshalizable? test"
     (for-each (lambda (obj)
                 (assert-true (marshalizable? obj)))
               (list 1 1.0 'a "a" #t #f :a '() #()
                     (list 1 1.0 'a "a" #t #f :a '() #())
                     (list '(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                     #(1 1.0 'a "a" #t #f :a '() #())
                     (list '(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                     #('(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                     (list (lambda () #f))
                     (list 1.0 (lambda () #f) 'a "a")
                     )))
    ("can't marshalizable? test"
     (for-each (lambda (obj)
                 (assert-false (marshalizable? obj)))
               (list (lambda () #f)
                     (with-module dsm.marshal (make <reference-object>))
                     )))
    ("marshal/unmarshal test"
     (for-each (lambda (obj)
                 (assert-equal obj (unmarshal table
                                              (read-from-string
                                               (marshal table obj)))))
               (list 1 'abc "a" '(1) #()
                     (lambda () #f)
                     (make-hash-table)
                     (with-module dsm.marshal
                       (make <reference-object>
                         :id 1
                         :host "example.com"
                         :port 5963))
                     (list 1 (lambda (x) x) '(1)))))
    ))
