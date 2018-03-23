(define-module
  (engine)
  ;; #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-26)
  ;; #:use-module (oop goops)
  ;; #:use-module (oop goops describe)
  ;; #:use-module (scene)
  ;; #:use-module (vector)
  ;; #:use-module (util)
  ;; #:use-module (event) ; event-func

  #:use-module (oop goops)
  #:use-module (util)
  #:use-module (object)

  #:export (
            ready!
            draw-rect
            set-color
            draw-text
            draw-line
            draw-ellipse
            load-image
            render-texture
            render-sprite
            texture-size

            draw-func
            tick-func
            collide-func
            )
  )

(load-extension "./main" "init_functions")

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

;;; TODO This should be defined in objects/text-obj.scm
;; (define-class <text-obj> (<geo-object>)
;;               (text #:init-value " "
;;                     #:init-keyword #:str)
;;               (update-text #:init-value #f
;;                            #:init-keyword #:update))



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


(define-generic tick-func)
(define-method (tick-func (obj <game-object>))
  (slot-mod! obj 'counter 1+))

(define-generic draw-func)
(define-method (draw-func (obj <geo-object>)))

(define-generic collide-func)
(define-method (collide-func (obj-a <geo-object>)
                             (obj-b <geo-object>))
  #f)
