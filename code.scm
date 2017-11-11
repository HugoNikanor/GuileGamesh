(use-modules (srfi srfi-1)
             (oop goops)
             (oop goops describe))

(define-class <game-object> ()
              (c #:init-value 0 #:getter counter))

(define-class <geo-object> (<game-object>)
              (x #:accessor obj-x #:init-value 0)
              (y #:accessor obj-y #:init-value 0))

(define-class <box> (<geo-object>)
              (w #:init-value 10)
              (h #:init-value 10))

(define-method (inc-counter! (box <box>))
  (slot-set! box 'c (1+ (counter box))))

(define-method (slide! (box <box>))
               (set! (obj-x box) (1+ (obj-x box)))
               (set! (obj-y box) (1+ (obj-y box))))

(define-generic tick-func)
(define-method (tick-func (box <box>))
  (inc-counter! box)
  (when (zero? (remainder (counter box)
                          1000))
    (slide! box)))

(define ev '())

(define-class <event> ()
              type timestamp window-id)
(define-class <key-event> (<event>) state repeat scancode sym mod)
(define-class <mouse-btn-event> (<event>) which state clicks
              (button #:getter mouse-button)
              (x #:getter mouse-x)
              (y #:getter mouse-y))

(define-generic fix-event-args)
(define-method (fix-event-args (_ <event>) rest))
(define-method (fix-event-args (ev <key-event>) state repeat keysym)
               (apply (lambda (scancode sym mod)
                        (slot-set! ev 'state state)
                        (slot-set! ev 'repeat repeat)
                        (slot-set! ev 'scancode (list-ref keysym 0))
                        (slot-set! ev 'sym (list-ref keysym 1))
                        (slot-set! ev 'mod (list-ref keysym 2)))
                      keysym))
(define-method (fix-event-args (ev <mouse-btn-event>)
                               which button state clicks x y)
               (slot-set! ev 'which which)
               (slot-set! ev 'button button)
               (slot-set! ev 'state state)
               (slot-set! ev 'clicks clicks)
               (slot-set! ev 'x x)
               (slot-set! ev 'y y))


(define (event-func event)
  "Event dispatcher?"
  (apply (lambda (_ __ event-objs)
           (let ((e (eval `(make ,(car event))
                          (current-module))))
             (slot-set! e 'type (list-ref event 1))
             (slot-set! e 'timestamp (list-ref event 2))
             (slot-set! e 'window-id (list-ref event 3))
             (apply fix-event-args e (drop event 4))
             (for-each (lambda (obj)
                         (event-do obj e))
                       event-objs)))
         (get-registered-objects)))

(define-method (box-reset! (box <box>))
               (slot-set! box 'x 0)
               (slot-set! box 'y 0))


(define-generic event-do)


(define-method (event-do (obj <game-object>)
                         (event <event>))
               (set! ev event))

(define-method (event-do (box <box>)
                         (event <key-event>))
               (when (and (eqv? (slot-ref event 'type)
                                'SDL_KEYDOWN)
                          (= (slot-ref event 'sym)
                             #x20))
                 ;;(box-reset! box))
                 (describe event))
               (next-method))

(define-method (event-do (box <box>)
                         (event <mouse-btn-event>))
               (slot-set! box 'x (mouse-x event))
               (slot-set! box 'y (mouse-y event))
               (next-method))

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
