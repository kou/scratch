(define-module scratch.common
  (use www.cgi)
  (use srfi-1)
  (use srfi-11)
  (use gauche.parameter)
  (use gauche.sequence)
  (use scratch.session)
  (export *scratch-id-key* *scratch-user-key*
          *scratch-password-key* *scratch-action-key*
          session parameters
          get-param get-state set-state!
          get-id get-action get-user
          generate-id&action
          login! logout! login?)
  )
(select-module scratch.common)

(define *scratch-id-key* :__scratch_id__)
(define *scratch-user-key* :__scratch_user__)
(define *scratch-password-key* :__scratch_password__)
(define *scratch-action-key* :__scratch_action__)

(define *scratch-default-action-name* 'default)
(define *scratch-default-view-name* 'default-view)
(define *scratch-deny-action-name* 'deny)
(define *scratch-new-session-id* 0)

(define session (make-parameter #f))
(define parameters (make-parameter '()))

(define (get-param keyword . options)
  (apply cgi-get-parameter (x->string keyword) (parameters)
         `(,@(if (or (null? options)
                     (not (null? (cdr options))))
                 options
                 (list :default (car options)))
           :list #f)))

(define (get-state key . fall-back)
  (apply get-value (session) key fall-back))

(define (set-state! key value)
  (set-value! (session) key value))

(define (get-id)
  (get-response-value (session) *scratch-id-key*))

(define (get-action)
  (get-param *scratch-action-key*))

(define (get-user)
  (get-state *scratch-user-key* #f))

(define (generate-id&action . args)
  (let loop ((id (get-id))
             (action *scratch-default-action-name*)
             (args args))
    (cond ((null? args) (values id action args))
          ((memq (car args) '(:new-session :action))
           => (lambda (keywords)
                (case (car keywords)
                  ((:new-session) (loop (if (cadr args)
                                            *scratch-new-session-id*
                                            id)
                                        action
                                        (cddr args)))
                  ((:action) (loop id (cadr args) (cddr args))))))
          (else (let ((ind (find-index (cut memq <> '(:new-session :action))
                                       args)))
                  (values id action
                          (if ind
                              (let-values (((before after) (split-at args ind)))
                                `(,@(take after 2)
                                  ,@before
                                  ,@(cddr after)))
                              args)))))))
(define (login! user)
  (set-state! *scratch-user-key* user))

(define (logout!)
  (set-state! *scratch-user-key* #f))

(define (login? user)
  (let ((current-user (get-user)))
    (and (string? user)
         (string? current-user)
         (string=? user current-user))))

(provide "scratch/common")
