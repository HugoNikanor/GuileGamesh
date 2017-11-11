(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (oop goops)
             (oop goops describe)
             (scene)
             (vector)
             (util)
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

;; This pushes to far
(define-method (collide (a <colliding>)
                        (b <colliding>))
               (format #t "~s and ~s collided\n" (object-name a) (object-name b))
               (slot-mod! b 'pos
                          (cut + <> (* (1- (slot-ref b 'friction))
                                       (- (+ (* 1/2 (size a))
                                             (pos a))
                                          (+ (* 1/2 (size b))
                                             (pos b)))))))

(define-method (colliding? (a <colliding>)
                           (b <colliding>))
               (let ((v (pos a))
                     (u (pos b)))
                 (and 
                   (not (> (x v) (+ (x u) (x (size b)))))
                   (not (> (x u) (+ (x v) (x (size a)))))
                   (not (> (y u) (+ (y v) (y (size a)))))
                   (not (> (y v) (+ (y u) (y (size b))))))))

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
(slot-set! player-box 'friction 0.5)

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

(define-class <tile-set> (<game-object>)
              tile-sheet ; SDL_texture
              tile-size ; 16
              board-size ; v2 16 16 (in tiles)
              )

(define tileset (make <tile-set>))
(slot-set! tileset 'tile-size 16)
(slot-set! tileset 'board-size (make <v2> #:x 32 #:y 32))

(define tile-defs
  (make-array '(0 0) '(0 31) '(0 31)))

(define-method
  (draw-func (tileset <tile-set>))
  (apply-for-each
    (lambda (x y)
      (render-texture (slot-ref tileset 'tile-sheet)
                      (slot-ref tileset 'tile-size)
                      ;'(0 0) ; sprite position in sheet 
                      (array-ref tile-defs y x)
                      (list x y)))
    (cart-prod (iota (x (slot-ref tileset 'board-size)))
               (iota (y (slot-ref tileset 'board-size))))))

(define (next-tile tile)
  (let ((a (car tile))
        (b (cadr tile)))
    (if (= b 31)
      (list (1+ a) b)
      (list a (1+ b)))))

(define mouse-pos #f)

(define tiles-of-interest
  '((2 0)
    (2 1)
    (2 2)
    (1 0)
    (1 1)
    (1 2)
    (0 0)
    (0 1)
    (0 2)))

;; 49 - 57 sym
;; 89 - 97 u 30 - 39 scancode
(define-method
  (event-do (tileset <tile-set>)
            (event <key-event>))
  (when mouse-pos
    (let ((x (floor (/ (x mouse-pos) 16)))
          (y (floor (/ (y mouse-pos) 16))))
      (let ((n (slot-ref event 'scancode)))
        (if (> n 40)
          (set! n (- n 89))
          (set! n (- n 30)))
        (array-set! tile-defs (list-ref tiles-of-interest n)
                    y x))))
  (next-method))


(define-method
  (event-do (tileset <tile-set>)
            (event <mouse-motion-event>))
  (set! mouse-pos (make <v2> #:x (mouse-x event) #:y (mouse-y event)))
  (next-method))

(register-event-object! tileset)

;;(register-draw-object! other-debug)
(register-draw-object! box-pos)
(register-tick-object! box-pos)
;; Note that objects can be shared between scenes
;; (slot-set! other-debug 'text "Hello, World!")

(define fpath "/home/hugo/code/guile/engine/assets/PathAndObjects_0.png")
(define (init-tile-set)
  (slot-set! tileset 'tile-sheet
             (load-image fpath))
  (register-draw-object! tileset))

(define scene3 (make <scene> #:name "SCENE 3"))
(set-current-scene! scene3)

(define-class <tileset-grid> (<tileset>))

(define-method (draw-func (obj <tileset-grid>))

(ready!)
