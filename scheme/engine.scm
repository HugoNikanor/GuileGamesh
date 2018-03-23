(define-module (engine)
  #:use-module (oop goops)
  #:use-module (util)
  #:use-module (object)

  #:export (ready! draw-rect set-color draw-text
            draw-line draw-ellipse load-image
            render-texture render-sprite texture-size

            draw-func tick-func collide-func))

(load-extension "./main" "init_functions")

(define-generic tick-func)
(define-method (tick-func (obj <game-object>))
  (slot-mod! obj 'counter 1+))

(define-generic draw-func)
(define-method (draw-func (obj <geo-object>)))

(define-generic collide-func)
(define-method (collide-func (obj-a <geo-object>)
                             (obj-b <geo-object>))
  #f)
