(define-module scratch.common
  (export *scratch-store-mount-point* *scratch-restore-mount-point*)
  )
(select-module scratch.common)

(define *scratch-store-mount-point* "/store")
(define *scratch-restore-mount-point* "/restore")

(provide "scratch/common")
