#!/usr/bin/env gosh

(use test.unit)
(use dsm.marshal)

(define-test-case "Marshal test"
  ("can marshalizable? test"
   (for-each (lambda (obj)
               (assert-true (marshalizable? obj)))
             (list 1 1.0 'a "a" #t #f :a '() #()
                   (list 1 1.0 'a "a" #t #f :a '() #())
                   (list '(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                   #(1 1.0 'a "a" #t #f :a '() #())
                   (list '(1 1.0) 'a '("a" #(#t #f) :a) '() #())
                   #('(1 1.0) 'a '("a" #(#t #f) :a) '() #()))))
  ("can't marshalizable? test"
   (for-each (lambda (obj)
               (assert-false (marshalizable? obj)))
             (list (lambda () #f)
                   (list (lambda () #f))
                   (list 1.0 (lambda () #f) 'a "a"))))
  ("marshal/unmarshal test"
   (for-each (lambda (obj)
               (assert-equal obj (unmarshal (read-from-string (marshal obj)))))
             (list 1 'abc "a" '(1) #()
                   (lambda () #f)
                   (make-hash-table))))
  )
