#!/usr/bin/env gosh

(use gauche.parameter)
(use test.unit)
(use scratch.common)
(use scratch.session)

(require "test/util")

(define-macro (assert-each-value prepare-proc lists)
  `(assert-each-list-elem
    ,prepare-proc
    :assert-proc (lambda (key expected actual)
                   (assert-equal expected actual
                                 (with-module test.unit
                                   (make-message-handler
                                    expected
                                    :after-expected #`" when key=,|key|"))))
    :lists ,lists))

(define-test-case "scratch common test"
  ("get-param  test"
   (let ((params `(("int" 1)
                   ("string" "string")
                   ("symbol" sym)
                   ("list" (1 2 3) (4 5 6))
                   ("proc" ,(lambda () #f)))))
     (parameterize ((parameters params))
       (assert-each-list-elem (lambda (key value . options)
                                (list value (apply get-param key options)))
                              :lists `(("int" 1)
                                       (int (1) :list #t)
                                       (:string "string")
                                       ("string" string
                                        :convert ,string->symbol)
                                       ("list" (1 2 3))
                                       ("list" ((1 2 3) (4 5 6)) :list #t)
                                       (list 1 :list #f :convert ,car)
                                       (list (1 4) :list #t :convert ,car)
                                       (:proc #f :convert ,(cut apply <> '()))
                                       ("not-exist" #f)
                                       ("not-exist" () :list #t)
                                       ("not-exist" #t :default #t)
                                       )))))
   ("get-value/set-value! test"
    (parameterize ((session (make-scratch-session)))
      (assert-each-value (lambda (key value . options)
                           (if key (set-value! key value))
                           (list key value (apply get-value key options)))
                         `((#f 1 1)
                           (one 1 2)))
      (assert-each-value (lambda (key value)
                           (set-value! key value)
                           (list key value (get-value key)))
                         `((int 1)
                           (int 100)
                           (:string "string")
                           (list (1 2 3))
                           (:symbol sym)
                           (:proc ,(lambda () #f))
                           (true #t)
                           (false #f)))))
   ("get-id/set-id! test"
    (assert-equal #f (get-id (make-scratch-session)))
    (let ((ids '(1 0 -1 :a a)))
      (assert-each (lambda (id)
                     (let ((sess (make-scratch-session)))
                       (set-id! sess id)
                       (list id (get-id sess))))
                   ids)
      (let ((sess (make-scratch-session)))
        (assert-each (lambda (id)
                       (set-id! sess id)
                       (list id (get-id sess)))
                     ids))
      (let ((sess (make-scratch-session)))
        (assert-each (lambda (id)
                       (set! (id-of sess) id)
                       (set-id! sess)
                       (list id (get-id sess)))
                     ids))
      (parameterize ((session (make-scratch-session)))
        (assert-each (lambda (id)
                       (set! (id-of (session)) id)
                       (set-id!)
                       (list id (get-id)))
                     ids))
      (assert-each (lambda (id)
                     (parameterize ((session (make-scratch-session)))
                       (set! (id-of (session)) id)
                       (set-id!)
                       (list id (get-id))))
                   ids)))
   ("get-action test"
    (assert-false (get-action))
    (let ((key (x->string *scratch-action-key*)))
      (parameterize ((parameters `((,key "act"))))
        (assert-equal "act" (get-action)))
      (parameterize ((parameters `((,key "act1" "act2"))))
        (assert-equal "act1" (get-action)))))
   ("login test"
    (parameterize ((session (make-scratch-session)))
      (let ((user "user"))
        (assert-false (login? user))
        (login! user)
        (assert-true (login? user))
        (assert-false (login? "other-user"))
        (logout!)
        (assert-false (login? user))
        (assert-false (login? "other-user")))))
   ("get-user test"
    (parameterize ((session (make-scratch-session)))
      (assert-false (get-user))
      (let ((user "user"))
        (login! user)
        (assert-equal user (get-user)))))
   ("generate-id&action test"
    (for-each
     (lambda (id)
       (parameterize ((session (make-scratch-session)))
         (if id (set-id! (session) id))
         (assert-each-list-elem
          (lambda (expected . args)
            (call-with-values (cut apply generate-id&action args)
              (lambda actual (list expected actual))))
          :lists `(((,id default ()))
                   ((,id default ()) :new-session #f)
                   ((0 default ()) :new-session #t)
                   ((,id default ()) :new-session #t :new-session #f)
                   ((0 default ()) :new-session #f :new-session #t)
                   ((,id default ()) :new-session #f :new-session #f)
                   ((,id abc ()) :action abc)
                   ((,id def ()) :action abc :action def)
                   ((,id default (a)) a)
                   ((,id default (a)) :new-session #f a)
                   ((,id default (a)) a :new-session #f)
                   ((0 default (a)) a :new-session #t)
                   ((,id abc (a)) a :action abc)
                   ((,id def (a :other)) a :action abc :other :action def)
                   ((,id abc ()) :new-session #t :action abc :new-session #f)
                   ((0 abc (a b c)) a :action abc b :new-session #t c)))))
     '(#f 10 100)))
   )
