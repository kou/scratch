(define-module scratch.server
  (use dsm.server)
  (use dsm.marshal)
  (use dsm.common)
  (use scratch.common)
  (export make-scratch-server add-mount-point! get-by-mount-point
          store restore start-scratch-server stop-scratch-server)
  )
(select-module scratch.server)

(define-class <scratch-server> ()
  ((dsm-server :accessor dsm-server-of)
   (store-table :accessor store-table-of)))

(define-method initialize ((self <scratch-server>) args)
  (next-method)
  (let ((dsm-server (apply make-dsm-server args)))
    (add-mount-point! dsm-server *scratch-store-mount-point*
                      (cut store self <>))
    (add-mount-point! dsm-server *scratch-restore-mount-point*
                      (cut restore self <>))
    (slot-set! self 'dsm-server dsm-server))
  (slot-set! self 'store-table (make-marshal-table-using-socket
                                (socket-of (dsm-server-of self))))
  )

(define (make-scratch-server . keywords)
  (apply make <scratch-server> keywords))

(define-method add-mount-point! ((self <scratch-server>) mount-point obj)
  (add-mount-point! (dsm-server-of self) mount-point obj))

(define-method get-by-mount-point ((self <scratch-server>) mount-point)
  (get-by-mount-point (dsm-server-of self) mount-point))

(define-method store ((self <scratch-server>) obj)
  (id-get (store-table-of self) obj))

(define-method restore ((self <scratch-server>) id)
  (id-ref (store-table-of self) id))

(define (start-scratch-server server)
  (start-dsm-server (dsm-server-of server)))

(define (stop-scratch-server server)
  (stop-dsm-server (dsm-server-of server)))
  
(provide "scratch/server")