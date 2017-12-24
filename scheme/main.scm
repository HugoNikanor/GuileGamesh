(add-to-load-path "scheme")

;; TODO check which imports are actually needed
(use-modules (engine)
             (srfi srfi-1)
             (srfi srfi-26)
             (oop goops)
             (oop goops describe)
             (scene)
             (vector)
             (util))

(define-method (event-do (obj <game-object>)
                         (event <event>))
               (slot-set! other-debug 'text
                          (with-output-to-string
                            (lambda ()
                              (display (slot-ref event 'type))
                              (display " | ")
                              (display (object-name obj))))))

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

(define-method (draw-func (box <box>))
               (apply set-color (slot-ref box 'color))
               (let ((pos (pos box))
                     (size (size box)))
               (draw-rect #t
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

;;; ------------------------------------------------------------

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

(set-current-scene! scene1)

;;; (with-scene
;;;   scene2
;;;   (define enemy-box
;;;     (make <colliding>
;;;           #:name "[ENEMY]"
;;;           #:pos  (make <v2> #:x 10 #:y 100)
;;;           #:size (make <v2> #:x 10 #:y 10)))
;;;   (define player-box
;;;     (make <colliding>
;;;           #:name "[PLAYER]"
;;;           #:pos  (make <v2> #:x 100 #:y 10)
;;;           #:size (make <v2> #:x 10 #:y 10)
;;;           #:color '(0 0 #xFF)
;;;           #:friction 0.5)))

(with-scene
  scene2
  (define enemy-box
    (make <box>
          #:name "[ENEMY]"
          #:pos  (make <v2> #:x 10 #:y 100)
          #:size (make <v2> #:x 10 #:y 10)))
  (define player-box
    (make <box>
          #:name "[PLAYER]"
          #:pos  (make <v2> #:x 100 #:y 10)
          #:size (make <v2> #:x 10 #:y 10)
          #:color '(0 0 #xFF)
          #:friction 0.5)))

(set-current-scene! scene2)

(register-draw-object! enemy-box)
(register-draw-object! player-box)
(register-event-object! player-box)

(define arrow-up 82)
(define arrow-down 79)
(define arrow-left 81)
(define arrow-right 80)

(define-method (tick-func (box <box>)))

;;; (define-method (tick-func (obj <colliding>))
;;;   (for-each (lambda (other)
;;;               (when (and (not (eq? obj other))
;;;                          (colliding? obj other))
;;;                 (collide obj other)
;;;                 (collide other obj)))
;;;             (get-colliders (current-scene)))
;;;   (next-method))

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
        (cond
          ((< 30 n 39)
           (set! n (- n 30)))
          ((< 89 n 98)
           (set! n (- n 89))))
        (when (<= 1 n 9)
          (array-set! tile-defs (list-ref tiles-of-interest n)
                      y x)))))
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
  (register-draw-object! tileset scene2))

(define scene3 (make <scene> #:name "SCENE 3"))
(set-current-scene! scene3)

;;(define-class <tileset-grid> (<tile-set>))
(define-class <tileset-grid> ())

(define-method (draw-func (obj <tileset-grid>))
  (for-each (lambda (y)
              (for-each (lambda (x)
                          (draw-line 0 y 512 y)
                          (draw-line x 0 x 512))
                        (map (cut * <> 16) (iota 32))))
            (map (cut * <> 16) (iota 32)))
  ;;(next-method))
  )

(define ts-grid (make <tileset-grid>))

(register-draw-object! ts-grid)

(ready!)

(set-current-scene! scene2)
