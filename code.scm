(use-modules (oop goops)
             (oop goops describe))

(define-class <game-object> ())

(define-class <box> (<game-object>)
              (x #:init-value 0)
              (y #:init-value 0)
              (w #:init-value 10)
              (h #:init-value 10)
              (c #:init-value 0 #:getter counter))

(define-method (inc-counter! (box <box>))
  (slot-set! box 'c (1+ (counter box))))

(define-method (slide! (box <box>))
  (slot-set! box 'x (1+ (slot-ref box 'x)))
  (slot-set! box 'y (1+ (slot-ref box 'y))))

(define-generic tick-func)
(define-method (tick-func (box <box>))
  (inc-counter! box)
  (when (zero? (remainder (counter box)
                          1000))
    (slide! box)))

(define-generic event-func)
(define-method (event-func (box <box>)
                          event)
  (display 'event)
  (newline))

(define-generic draw-func)
(define-method (draw-func (box <box>))
  (draw-rect #f
    (slot-ref box 'x)
    (slot-ref box 'y)
    (slot-ref box 'w)
    (slot-ref box 'h)))

(define box (make <box>))

(register-draw-object! box)
(register-tick-object! box)
(register-event-object! box)
(ready!)
