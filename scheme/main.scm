(add-to-load-path "scheme")

(use-modules (engine)
             ;;(srfi srfi-1)
             (srfi srfi-26)
             (oop goops)
             (oop goops describe)
             (scene)
             (vector)
             (util)

             (event)              
             (event key)          
             (event mouse-motion) 
             (event mouse-btn)    

             (objects box)
             (objects ellipse)

             )

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

#|
(define-method (event-do (box <box>)
                        (event <key-event>))
               (slot-set! other-debug 'text
                           "huh?"))
|#

(define-method (draw-func (text <text-obj>))
               (draw-text (slot-ref text 'text)
                          (x (pos text))
                          (y (pos text))))

(define-method (tick-func (text <text-obj>))
               (let ((func (slot-ref text 'update-text)))
                 (when func
                   (slot-set! text 'text (func text)))))

;;; ------------------------------------------------------------
(define coll-text (make <text-obj>
                        #:pos (make <v2> #:y 5 #:x 5)
                        #:str "U"
                        #:update (lambda (_)
                                   (if (colliding? pel eel)
                                     "T" "F"))))

(register-draw-object! coll-text)
(register-tick-object! coll-text)

;;; ------------------------------------------------------------

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

(set-current-scene! scene2)

(define-class <ctrl-el> (<ellipse>))
(define pel
  (make <ctrl-el>
        #:name "[ELLIPSE 1]"
        #:pos (make <v2> #:x 40 #:y 40)
        #:r 100
        #:d 30))

(define eel
  (make <ellipse>
        #:name "[ELLIPSE 2]"
        #:pos (make <v2> #:x 120 #:y 120)
        #:color '(#xFF 0 0)
        #:r 100
        #:d 30))

(register-draw-object! pel)
(register-event-object! pel)
(register-tick-object! pel)

(register-draw-object! eel)
;;(register-event-object! eel)
;;(register-tick-object! eel)

(register-draw-object! enemy-box)
(register-draw-object! player-box)
(register-event-object! player-box)

(define arrow-up 82)
(define arrow-down 79)
(define arrow-left 81)
(define arrow-right 80)

(define-method (tick-func (box <box>)))

(define (square x)
  (* x x))

;;; (define (get-largest-by func . items)
;;;   (map (lambda (item)
;;;          (cons item (func item)))

(define-method (get-intersection-translation-vector
                 (el1 <ellipse>)
                 (el2 <ellipse>))
               ;; This is either right or wrong 
               ;; if wrong, add paj ?
               (let ((s2 (helperfunction el1 el2))
                     (s1 (helperfunction el2 el1)))
                 (let ((v (pos el1))
                       (u (pos el2)))
                   (* (- v u) 
                      (- (abs (- v u))
                         (abs (- v s2))
                         (abs (- u s1)))
                      (/ 1 (abs (- v u)))))))

(define pi 3.141592653589793)
;;; (define pi 42/13.37)

(define-method (helperfunction
                 (el1 <ellipse>)
                 (el2 <ellipse>))
               "Returns point"
               (let* ((help (x (- (pos el1)
                                  (pos el2))))
                      (theta (+ (if (> help 0) 0 pi)
                                (acos (/ (abs help)
                                         (abs (- (pos el1)
                                                 (pos el2)))))))
                      (r (slot-ref el1 'r))
                      (rx (/ r 2))
                      (ry (sqrt (- (square rx)
                                   (square (slot-ref el1 'd))))))
                 (+ (pos el1)
                    (make <v2>
                          #:x (* rx (cos theta))
                          #:y (* ry (sin theta))))))

;;(define-method (get-sub-points (el <ellipse>))
  

(define-method (get-sub-point-helper func (el <ellipse>))
  (func (pos el)
        (make <v2> #:x (slot-ref el 'd))))

#| for future use
(define-macro (swap! a b)
  (let ((symb (gensym)))
    `(begin
       (set! ,symb ,a)
       (set! 
         |#

(define-generic colliding?)
;;(define-method (colliding? <geo-object> <geo-object>))
(define-method (colliding?
                 (el1 <ellipse>)
                 (el2 <ellipse>))
               (let ((togeled? #f))
                 (let ((el1_a (get-sub-point-helper + el1))
                       (el1_b (get-sub-point-helper - el1))
                       (el2_a (get-sub-point-helper + el2))
                       (el2_b (get-sub-point-helper - el2)))
                   (let ((v (- el1_a el2_a))
                         (u (- el1_b el2_b)))
                     ;; (‡ 0)
                     (when (> 1e-30 (abs (real-part (- (* (x v)
                                                          (y u))
                                                       (* (y v)
                                                          (x u))))))
                       (set! togeled? #t)
                       (set! v (- el1_b el2_a))
                       (set! u (- el1_a el2_b)))

                     (if togeled?
                       (begin
                         (set-color #xFF 0 0) ; RED
                         (draw-line (x el1_a) (y el1_a)
                                    (x el2_b) (y el2_b))
                         (draw-line (x el1_b) (y el1_b)
                                    (x el2_a) (y el2_a)))
                       (begin
                         (set-color 0 #xFF 0) ; GREEN
                         (draw-line (x el1_a) (y el1_a)
                                    (x el2_a) (y el2_a))
                         (draw-line (x el1_b) (y el1_b)
                                    (x el2_b) (y el2_b))))
                       ;; TODO
                       (not (< (real-part (+ (slot-ref el1 'r)
                                             (slot-ref el2 'r)))
                               (real-part (+ (abs v)
                                             (abs u)))))))))
                     

;;; TODO Actually figure out what can collide
;;; It will probably only be a rigid tile set
;;; and actors wich will all be ellipses
(define-generic collide!)
;;(define-method (collide <geo-object>))
(define-method (collide! (el1 <ellipse>)
                         (el2 <ellipse>))
               "Mutates el1"
               (slot-mod! el1 'pos
                          (lambda (p)
                            (- p (catch 'numerical-overflow
                                        (lambda ()
                                          (get-intersection-translation-vector el1 el2))
                                        (lambda (symb . msg)
                                          (make <v2> #:x 1)))))))
               #|
               (catch 'numerical-overflow
                      (lambda ()
                        (slot-mod! el1 'pos
                                   (lambda (p)
                                     (- p (get-intersection-translation-vector el1 el2)))))
                      (lambda (error . message)
                        (slot-mod! (pos el1) 'x 1+))))
|#

;;(define f (lambda (x y) (list x y)))
(define-method (tick-func (obj <ctrl-el>))
               (let ((other eel))
                 (if (and (not (eq? obj other))
                          (colliding? obj other))
                   (collide! obj other))))
                   ;;(collide! obj other)))
                   ;;(describe #t)
                   ;;(describe #f))))
  ;;(for-each (lambda (other)
  ;;            ;;(if (colliding? obj other)
  ;;          ;; TODO should this be get-colliders
  ;;          (get-tick-list (current-scene))))

;;; (define-method (tick-func (obj <colliding>))
;;;   (for-each (lambda (other)
;;;               (when (and (not (eq? obj other))
;;;                          (colliding? obj other))
;;;                 (collide obj other)
;;;                 (collide other obj)))
;;;             (get-colliders (current-scene)))
;;;   (next-method))

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

(set-current-scene! scene2)
