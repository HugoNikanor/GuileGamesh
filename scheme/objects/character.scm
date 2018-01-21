(define-module (objects character)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (vector)
               #:use-module (objects vecloader)
               #:use-module (objects ellipse)
               #:export (<character>))

#| TODO
 | currently inheritance order dictates drawing order.
 | This is probably bad, since it will then be hard
 | to handle different layers in one model. But it
 | might be possible by mutating the list during runtime.
 |#
(define-class <character> (<ellipse> <vec-graphics>))

(define-method (draw-func (obj <character>))
               (next-method))

(define-method (initialize (this <character>) args)
               (next-method)
               (slot-set! this 'r 100)
               (slot-set! this 'd 30))

#|
(define (create-character str)
  "TODO It would be better if this was merged into
  the constructor for characters"
  (let ((obj (parse str)))
    (change-class obj <character>)
    (set! (pos obj) (make <v2> '(200 200)))
    (slot-set! obj 'r 20)
    (slot-set! obj 'd 10)
    obj))
               ;;; (for-each draw-func (class-direct-supers obj) )
|#
