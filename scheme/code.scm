(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (oop goops)
             (oop goops describe)
             (scene)
             (vector)
             )
             ;;(scheme scene))

(define (r)
  (system "reset"))

(define-macro (slot-mod! obj slot func)
  `(slot-set! ,obj ,slot (,func (slot-ref ,obj ,slot))))

(define-class <game-object> ()
              (name #:init-keyword #:name #:getter object-name #:init-value "[NAMELESS]")
              (c #:init-value 0 #:getter counter))

(define-class <geo-object> (<game-object>)
              (pos #:accessor pos #:init-keyword #:pos))
              ;;(x #:init-keyword #:x #:accessor obj-x #:init-value 0)
              ;;(y #:init-keyword #:y #:accessor obj-y #:init-value 0))

(define-class <box> (<geo-object>)
              (size #:accessor size #:init-keyword #:size)
              (color #:init-keyword #:color #:init-value '(#xFF 0 0)))

(define-class <colliding> (<box>)
              (friction #:init-value 1))

(define (make-colliding)
  (let ((obj (make <colliding>)))
    (register-collider! obj)
    (register-tick-object! obj)
    obj))

(define-method (collide (a <colliding>)
                        (b <colliding>))
               ;;(format #t "~s and ~s collided\n" (object-name a) (object-name b)))
               (slot-mod! b 'pos
                          (cut + <> (* (1- (slot-ref b 'friction))
                                       (- (pos b) (pos a))))))

(define-method (colliding? (a <colliding>)
                           (b <colliding>))
               (let ((v (pos a))
                     (u (pos b)))
                 (or (and (< (x v) (y v) (+ (x v) (x (size a))))
                          (< (y v) (y u) (+ (y v) (y (size a)))))
                     (and (< (x u) (x u) (+ (x u) (x (size b))))
                          (< (y u) (y u) (+ (y u) (y (size b))))))))
                 #|
                 (or (and (< (x v) (y v) (+ (x v) (x (size v))))
                          (< (y v) (y u) (+ (y v) (y (size v)))))
                     (and (< (x u) (x a) (+ (x u) (x (size u))))
                          (< (y u) (y a) (+ (y u) (y (size u))))))))
|#

(define-class <text-obj> (<geo-object>)
              (text #:init-value " ")
              (update-text #:init-value #f))

(define-method (slide! (box <box>))
               (slot-mod! (pos box) 'x 1+)
               (slot-mod! (pos box) 'y 1+))
               ;;(slot-set! box 'x (1+ (slot-ref box 'x)))
               ;;(slot-set! box 'y (1+ (slot-ref box 'y))))

(define-generic tick-func)

(define-method (tick-func (obj <game-object>))
               ;;(slot-set! obj 'c (1+ (slot-ref obj 'c))))
               (slot-mod! obj 'c 1+))

(define-method (tick-func (box <box>))
               (when (zero? (remainder (counter box)
                                       1000))
                 (slide! box))
               (next-method))

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


#|
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
         (current-scene)))
|#
(define (event-func event)
  (let ((e (eval `(make ,(car event))
                 (current-module))))
    (slot-set! e 'type (list-ref event 1))
    (slot-set! e 'timestamp (list-ref event 2))
    (slot-set! e 'window-id (list-ref event 3))
    (apply fix-event-args e (drop event 4))
    (for-each (lambda (obj)
                (event-do obj e))
              (get-event-list (current-scene)))))

#|
(define* (collision-check #:optional (scene (current-scene)))
         (define (inner rem)
           (let ((item (car rem)))
             (for-each (lambda (other)
                         (when (colliding? item other)
                           (collide a b)
                           (collide b a)))
                       (cdr rem)))
           (inner (cdr rem)))
         (inner (get-colliders scene)))
|#

(define-method (box-reset! (box <box>))
               (slot-set! box 'x 0)
               (slot-set! box 'y 0))

(define-generic event-do)

(define-method (event-do (obj <game-object>)
                         (event <event>))
               (slot-set! other-debug 'text
                          (with-output-to-string
                            (lambda ()
                              (display (slot-ref event 'type))
                              (display " | ")
                              (display (object-name obj)))))
               (set! ev event))

#|
(define-method (event-do (box <box>)
                        (event <key-event>))
               (slot-set! other-debug 'text
                           "huh?"))
|#


(define-method (event-do (box <box>)
                         (event <key-event>))
               (when (and (eqv? (slot-ref event 'type)
                                'SDL_KEYDOWN)
                          (= (slot-ref event 'sym)
                             #x20))
                 (box-reset! box))
                 ;;(describe event))
               (next-method))

(define-method (event-do (box <box>)
                         (event <mouse-btn-event>))
               (slot-set! (pos box) 'x (mouse-x event))
               (slot-set! (pos box) 'y (mouse-y event))
               (next-method))

(define-generic draw-func)
(define-method (draw-func (box <box>))
               (apply set-color (slot-ref box 'color))
               (let ((pos (pos box))
                     (size (size box)))
               (draw-rect #f
                          (x pos)
                          (y pos)
                          (x size)
                          (y size))))

(define-method (draw-func (text <text-obj>))
               (draw-text (slot-ref text 'text)
                          (x (pos text))
                          (y (pos text))))

(define-method (tick-func (text <text-obj>))
               (let ((func (slot-ref text 'update-text)))
                 (when func
                   (slot-set! text 'text (func text)))))

(define box (make <box> #:name "[MAIN BOX]"))
(define box-pos (make <text-obj> #:pos (make <v2>)))
(slot-set! box-pos 'update-text
           (lambda (text)
             (format #f "~d ~d"
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

(define input-listener (make <game-object> #:name "[INPUT LISTENER]"))
(register-event-object! input-listener)

(slot-set! (current-scene) 'name "SCENE 1")
(define scene1 (current-scene))
(define scene2 (make <scene> #:name "SCENE 2"))

(set-current-scene! scene2)

;;(define enemy-box (make <box> #:x 10 #:y 10 #:color '(#xFF 0 0)))
;;(define player-box (make <box> #:x 100 #:y 100 #:color '(0 0 #xFF)))
(define enemy-box (make-colliding))
(define player-box (make-colliding))
(slot-set! enemy-box 'pos (make <v2> #:x 10 #:y 100))
(slot-set! enemy-box 'size (make <v2> #:x 10 #:y 10))
(slot-set! player-box 'pos (make <v2> #:x 100 #:y 10))
(slot-set! player-box 'size (make <v2> #:x 10 #:y 10))
(slot-set! player-box 'color '(0 0 #xFF))
(slot-set! player-box 'name "[PLAYER]")
(slot-set! enemy-box  'name "[ENEMY]")
;(slot-set! player-box 'pos (make <v2> #:x 20 #:y 20))
;(slot-set! player-box 'friction 0.5)

(register-draw-object! enemy-box)
(register-draw-object! player-box)
(register-event-object! player-box)

(define arrow-up 82)
(define arrow-down 79)
(define arrow-left 81)
(define arrow-right 80)

(define-method (tick-func (box <box>)))
(define-method (tick-func (obj <colliding>))
  (for-each (lambda (other)
              (when (and (not (eq? obj other))
                         (colliding? obj other))
                (collide obj other)
                (collide other obj)))
            (get-colliders (current-scene)))
  (next-method))
(define-method (event-do (obj <box>)
                         (event <key-event>))
               ;;;;(display (slot-ref event 'scancode))
               (case (slot-ref event 'scancode)
                 ((82)  (slot-mod! (pos obj) 'y 1- )) ;;
                 ((81)  (slot-mod! (pos obj) 'y 1+ ))
                 ((79)  (slot-mod! (pos obj) 'x 1+ )) ;;
                 ((80)  (slot-mod! (pos obj) 'x 1- ))
                 (else #f))
               (next-method))

;;(register-draw-object! other-debug)
(register-draw-object! box-pos)
(register-tick-object! box-pos)
;; Note that objects can be shared between scenes
;; (slot-set! other-debug 'text "Hello, World!")

;;(ready!)
