(define-module scratch.server
  (extend dsm.server scratch.scratch)
  (use marshal)
  (use dsm.common)
  (use scratch.servlet)
  (export make-scratch-server add-mount-point! get-by-mount-point
          scratch-server-start! scratch-server-join! scratch-server-stop!))
(select-module scratch.server)

(define-class <scratch-server> ()
  ((dsm-server :accessor dsm-server-of)))

(define-method initialize ((self <scratch-server>) args)
  (next-method)
  (set! (dsm-server-of self) (apply make-dsm-server args)))

(define (make-scratch-server uri . keywords)
  (apply make <scratch-server> uri  keywords))

(define-method add-mount-point! ((self <scratch-server>) mount-point servlet)
  (add-mount-point! (dsm-server-of self) mount-point
                    (cut dispatch servlet <> <> <> <...>)))

(define-method get-by-mount-point ((self <scratch-server>) mount-point)
  (get-by-mount-point (dsm-server-of self) mount-point))

(define (scratch-server-start! server)
  (dsm-server-start! (dsm-server-of server)))

(define (scratch-server-join! server)
  (dsm-server-join! (dsm-server-of server)))

(define (scratch-server-stop! server)
  (dsm-server-stop! (dsm-server-of server)))
  
(provide "scratch/server")
