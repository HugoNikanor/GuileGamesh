(define-module
  (engine)
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

            box-reset!
            draw-func
            tick-func
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

(define-generic draw-func)
