#|
 | This file is for setting up the repl environment, 
 | as well as starting the system.
 | 
 | ready! ought to be called from the C main code,
 | but I haven't got that to work yet.
 |#
(add-to-load-path "scheme")

#|
(use-modules (engine)
             ;;(srfi srfi-1)
             ;;(srfi srfi-26)
             (oop goops)
             (oop goops describe)
             (scene)
             ;;(vector)
             (util)
             (collide)

             (event)
             (event key)
             (event mouse-motion)
             (event mouse-btn)

             ;;(objects box)
             ;;(objects ellipse)

             (scene scene2)

             )
|#

(use-modules (scene scene2)
             (engine)
             (event) ;; event-func
             ;; These three are for event-func to work
             (event key)
             (event mouse-btn)
             (event mouse-motion))


#|
(define-method (event-do (box <box>)
                        (event <key-event>))
               (slot-set! other-debug 'text
                           "huh?"))
|#

;;; These should be moved to objects/text.scm

#|
(define-method (draw-func (text <text-obj>))
               (draw-text (slot-ref text 'text)
                          (x (pos text))
                          (y (pos text))))

(define-method (tick-func (text <text-obj>))
               (let ((func (slot-ref text 'update-text)))
                 (when func
                   (slot-set! text 'text (func text)))))

|#
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------


;; (define input-listener (make <game-object> #:name "[INPUT LISTENER]"))
;; (register-event-object! input-listener)

;(slot-set! (current-scene) 'name "SCENE 1")

;(define scene1 (current-scene))
;(define scene2 (make <scene> #:name "SCENE 2"))

;(set-current-scene! scene1)


;(set-current-scene! scene2)



#|
;; TODO this only works in scene2
;;;(define-method (event-do (obj <ctrl-box>)
(define-method (event-do (obj <ctrl-el>)
                         (event <key-event>))
               ;;;;(display (slot-ref event 'scancode))
               (case (slot-ref event 'scancode)
                 ((82)  (slot-mod! (pos obj) 'y 1- )) ;;
                 ((81)  (slot-mod! (pos obj) 'y 1+ ))
                 ((79)  (slot-mod! (pos obj) 'x 1+ )) ;;
                 ((80)  (slot-mod! (pos obj) 'x 1- ))
                 (else #f))
               (next-method))
|#

;;; ------------------------------------------------------------

#|
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
  (set! mouse-pos (make <v2> #:x (mousem-x event) #:y (mousem-y event)))
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

|#

(ready!)

;; (set-current-scene! scene2)
