(define-module
  (objects box)
  #:use-module (oop goops)
  #:use-module (engine)
  #:use-module (util)
  #:use-module (vector)

  #:use-module (event mouse-btn)
  #:export (
            <box> size
            slide! box-reset!))

(define-class <box> (<geo-object>)
              (size #:accessor size
                    #:init-keyword #:size)
              (color #:init-keyword #:color
                     #:init-value '(#xFF 0 0)))

(define-method (slide! (box <box>))
               (slot-mod! (pos box) 'x 1+)
               (slot-mod! (pos box) 'y 1+))
               ;;(slot-set! box 'x (1+ (slot-ref box 'x)))
               ;;(slot-set! box 'y (1+ (slot-ref box 'y))))

(define-method (box-reset! (box <box>))
               (slot-set! box 'x 0)
               (slot-set! box 'y 0))

#|
(define-method (event-do (box <box>)
                         (event <key-event>))
               (when (and (eqv? (slot-ref event 'type)
                                'SDL_KEYDOWN)
                          (= (slot-ref event 'sym)
                             #x20))
                 (box-reset! box))
                 ;;(describe event))
               (next-method))
|#

;; TODO this currently does nothing
(define-method (event-do (box <box>)
                         (event <mouse-btn-event>))
               (slot-set! (pos box) 'x (mouseb-x event))
               (slot-set! (pos box) 'y (mouseb-y event))
               (next-method))

(define-method (draw-func (box <box>))
               (apply set-color (slot-ref box 'color))
               (let ((pos (pos box))
                     (size (size box)))
               (draw-rect #t
                          (x pos)
                          (y pos)
                          (x size)
                          (y size))))
