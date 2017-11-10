(use-modules (oop goops)
             (oop goops describe))

(define-class <game-object> ())

(define-class <box> (<game-object>)
              (x #:init-value 0)
              (y #:init-value 0)
              (w #:init-value 10)
              (h #:init-value 10))


(define-generic obj-disp)
(define-method (obj-disp (box <box>))
  (describe box)
  (newline))

(define box (make <box>))

(register-object! box)
(ready!)
