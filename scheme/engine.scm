(define-module
  (engine)
  ;; TODO check which imports are actually needed
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (scene)
  #:use-module (vector)
  #:use-module (util)

  #:export (
            ready!
            draw-rect
            set-color
            draw-text
            draw-line
            load-image
            render-texture

            <game-object> object-name counter
            <geo-object> pos
            <box> size
            <text-obj>

            slide!

            tick-func

            <event>
            <key-event>
            <mouse-btn-event> mouse-button mouse-x mouse-y
            <mouse-motion-event> mouse-x mouse-y mouse-dx mouse-dy

            box-reset!
            event-do
            draw-func
            tick-func

            last-event

            ;; This needs to be exported for the sdl c code?
            event-func
            )
  )

(load-extension "./main" "init_functions")

;;; Primitive Data-objects
(define-class <game-object> ()
              (name #:init-keyword #:name
                    #:getter object-name
                    #:init-value "[NAMELESS]")
              (c #:init-value 0
                 #:getter counter))

(define-class <geo-object> (<game-object>)
              (pos #:accessor pos
                   #:init-keyword #:pos))

(define-class <box> (<geo-object>)
              (size #:accessor size
                    #:init-keyword #:size)
              (color #:init-keyword #:color
                     #:init-value '(#xFF 0 0)))

#| Colliding

Collision detection should be rethought and reimplemented
in the C part of the program.
;;; (define-class <colliding> (<box>)
;;;               (friction #:init-keyword #:friction
;;;                         #:init-value 1))
;;; 
;;; (define-method (initialize (obj <colliding>) initargs)
;;;                (register-collider! obj)
;;;                (register-tick-object! obj)
;;;                (next-method))
;;; 
;;; ;; This pushes to far
;;; (define-method (collide (a <colliding>)
;;;                         (b <colliding>))
;;;                (format #t "~s and ~s collided\n" (object-name a) (object-name b))
;;;                (slot-mod! b 'pos
;;;                           (cut + <> (* (1- (slot-ref b 'friction))
;;;                                        (- (+ (* 1/2 (size a))
;;;                                              (pos a))
;;;                                           (+ (* 1/2 (size b))
;;;                                              (pos b)))))))
;;; 
;;; (define-method (colliding? (a <colliding>)
;;;                            (b <colliding>))
;;;                (let ((v (pos a))
;;;                      (u (pos b)))
;;;                  (and
;;;                    (not (> (x v) (+ (x u) (x (size b)))))
;;;                    (not (> (x u) (+ (x v) (x (size a)))))
;;;                    (not (> (y u) (+ (y v) (y (size a)))))
;;;                    (not (> (y v) (+ (y u) (y (size b))))))))
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

;; Should really be of type Maybe Event
(define last-event '())

(define-class <event> ()
              type timestamp window-id)
(define-class <key-event> (<event>) state repeat scancode sym mod)
(define-class <mouse-btn-event> (<event>) which state clicks
              (button #:getter mouse-button)
              (x #:getter mouse-x)
              (y #:getter mouse-y))

(define-class <mouse-motion-event> (<event>)
              which state
              (x #:getter mouse-x)
              (y #:getter mouse-y)
              (xrel #:getter mouse-dx)
              (yrel #:getter mouse-dy))

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
(define-method (fix-event-args (ev <mouse-motion-event>)
                               which state x y yrel xrel)
               (slot-set! ev 'which which)
               (slot-set! ev 'state state)
               (slot-set! ev 'x x)
               (slot-set! ev 'y y)
               (slot-set! ev 'xrel xrel)
               (slot-set! ev 'yrel yrel))


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
  (set! last-event event)
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
(define-generic draw-func)
