(define marshalizable-key&value-alist
  ; (mount-point . value)
  '(("integer" . 1)
    ("string" . "str")
    ("symbol" . sym)
    ("list" . (1 "str" sym))
    ("vector" . #(1 "str" sym))
    ))

(define procedure-list
  ; (mount-point proc expected arg ...)
  `(("procedure0" ,(lambda () #t) #t)
    ("procedure1" ,(lambda (x) (+ x 2)) 3 1)
    ("procedure2" ,(lambda (x y) (+ x y)) 3 1 2)
;    ("procedure3" ,(lambda (proc x) (proc x)) 3 ,(lambda (x) (+ 1)) 2)
    ))
