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

(define ev '())

(define-class <event> ())
(define-class <key-event> (<event>) slots)

(define (event-func event)
  "Event dispatcher?"
  (apply (lambda (_ __ event-objs)
           (let ((e (case (car event)
                      ((<key-event>)
                       (let ((obj (make <key-event>)))
                         (slot-set! obj 'slots (cdr event))
                         obj))
                      (else
                        (make <event>)))))
             (for-each (lambda (obj)
                         (event-do obj e))
                       event-objs)))
         (get-registered-objects)))

(define-method (box-reset! (box <box>))
               (slot-set! box 'x 0)
               (slot-set! box 'y 0))



(define-generic event-do)
(define-method (event-do (obj <game-object>)
                         (event <event>)))

(define-method (event-do (box <box>)
                         (event <key-event>))
               (set! ev event)
               (apply (lambda (type time _ state repeat keysym)
                        (apply (lambda (scan sym mod)
                                 (when (= sym #x20)
                                   (box-reset! box)))
                               keysym)
                        (when (eqv? type 'SDL_KEYDOWN)
                          (describe event)))
                      (slot-ref event 'slots)))

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
