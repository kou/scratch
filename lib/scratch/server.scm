(define-module scratch.server
  (extend scratch.scratch)
  (use marshal)
  (use dsm.server)
  (use dsm.common)
  (use scratch.servlet)
  (export make-scratch-server add-mount-point! get-by-mount-point
          start-scratch-server stop-scratch-server)
  )
(select-module scratch.server)

(define-class <scratch-server> ()
  ((dsm-server :accessor dsm-server-of)))

(define-method initialize ((self <scratch-server>) args)
  (next-method)
  (slot-set! self 'dsm-server (apply make-dsm-server args)))

(define (make-scratch-server . keywords)
  (apply make <scratch-server> keywords))

(define-method add-mount-point! ((self <scratch-server>) mount-point servlet)
  (add-mount-point! (dsm-server-of self) mount-point
                    (cut dispatch servlet <> <> <> <...>)))

(define-method get-by-mount-point ((self <scratch-server>) mount-point)
  (get-by-mount-point (dsm-server-of self) mount-point))

(define (start-scratch-server server)
  (start-dsm-server (dsm-server-of server))
  (dsm-server-join! (dsm-server-of server)))

(define (stop-scratch-server server)
  (stop-dsm-server (dsm-server-of server)))
  
(provide "scratch/server")
