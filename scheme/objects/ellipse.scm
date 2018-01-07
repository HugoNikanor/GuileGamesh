(define-module
  (objects ellipse)
  #:use-module (oop goops)
  #:use-module (engine)
  #:use-module (util)
  #:use-module (vector)

  #:export (<ellipse>))

(define-class <ellipse> (<geo-object>)
              (r #:init-keyword #:r)
              (d #:init-keyword #:d)
              (color #:init-keyword #:color
                     #:init-value '(0 #xFF 0)))

(define-method (draw-func (el <ellipse>))
               (apply set-color (slot-ref el 'color))
               (draw-ellipse
                 (slot-ref el 'r)
                 (inexact->exact (floor (x (pos el))))
                 (inexact->exact (floor (y (pos el))))
                 (slot-ref el 'd)))
