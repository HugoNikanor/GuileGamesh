(define-module (scene scene1)
               #:export ())

;; Slide box doesn't work at the moment
(define-class <slide-box> (<box>))
(define-class <ctrl-box> (<box>))

(define-method (event-do (box <slide-box>)
                         (event <key-event>))
               (when (and (eqv? (slot-ref event 'type)
                                'SDL_KEYDOWN)
                          (= (slot-ref event 'sym)
                             #x20))
                 (box-reset! box))
                 ;;(describe event))
               (next-method))

(define-method (tick-func (box <slide-box>))
               (when (zero? (remainder (counter box)
                                       1000))
                 (slide! box))
               (next-method))

(define-method (event-do (box <ctrl-box>)
                         (event <mouse-btn-event>))
               (slot-set! (pos box) 'x (mouseb-x event))
               (slot-set! (pos box) 'y (mouseb-y event))
               (next-method))

(define-method (event-do (obj <game-object>)
                         (event <event>))
               (slot-set! other-debug 'text
                          (with-output-to-string
                            (lambda ()
                              (display (slot-ref event 'type))
                              (display " | ")
                              (display (object-name obj))))))

(define box (make <slide-box> #:name "[MAIN BOX]"))
(define box-pos (make <text-obj> #:pos (make <v2>)))
(slot-set! box-pos 'update-text
           (lambda (text)
             (format #f "~a ~a"
                     (x (pos player-box))
                     (y (pos player-box)))))

(define other-debug (make <text-obj> #:pos (make <v2> #:y 12)))

(register-draw-object! box)
(register-tick-object! box)
(register-event-object! box)

(register-draw-object! box-pos)
(register-tick-object! box-pos)

(register-draw-object! other-debug)
;;;(register-tick-object! other-debug)

(with-scene
  scene2
  (define enemy-box
    (make <ctrl-box>
          #:name "[ENEMY]"
          #:pos  (make <v2> #:x 10 #:y 100)
          #:size (make <v2> #:x 10 #:y 10)))
  (define player-box
    (make <ctrl-box>
          #:name "[PLAYER]"
          #:pos  (make <v2> #:x 100 #:y 10)
          #:size (make <v2> #:x 10 #:y 10)
          #:color '(0 0 #xFF)
          #:friction 0.5)))

(register-draw-object! enemy-box)
(register-draw-object! player-box)
(register-event-object! player-box)

