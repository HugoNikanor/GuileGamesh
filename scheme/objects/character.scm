(define-module (objects character)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (vector)
               #:use-module (objects vecloader)
               #:use-module (objects ellipse)
               #:export (<character>))

(define-class <character> (<vec-graphics> <ellipse>))

(define-method (draw-func (obj <character>))
               (next-method))

(define-method (initialize (this <character>) args)
               (set! (pos this) (make <v2> '(200 200)))
               (slot-set! this 'r 50)
               (slot-set! this 'd 30)
               (next-method))

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
