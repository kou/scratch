(define marshalizable-key&value-alist
  ; (mount-point . value)
  '(("integer" . 1)
    ("string" . "str")
    ("symbol" . sym)
    ("list" . (1 "str" sym))
    ("vector" . #(1 "str" sym))
    ))

(define procedure-list
  ; (mount-point value expected arg ...)
  `(("procedure0" ,(lambda () #f) #f)
    ("procedure1" ,(lambda (x) (+ x 2)) 3 1)
    ("procedure2" ,(lambda (x y) (+ x y)) 3 1 2)
    ))
