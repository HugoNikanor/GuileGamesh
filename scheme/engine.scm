;;; This is where the C code is sourced,
;;; and also where the main procedures get
;;; their declarations.
;;; I belive this is also where the graphical
;;; window is spawned, but I'm not sure

(define-module (engine)
  #:use-module (oop goops)
  #:use-module (util)
  #:use-module (objects)

  #:export (ready! draw-rect set-color draw-text
            draw-line draw-ellipse load-image
            render-texture render-sprite texture-size

            draw-func tick-func collide-func

            set-window-size!
            ))

(load-extension "./main" "init_functions")

(define set-color set-color!)
(define draw-rect primitive-draw-rect!)
(define draw-text primitive-draw-text!)
(define draw-line primitive-draw-line!)
(define draw-ellipse primitive-draw-ellipse!)
(define draw-pixel primitive-draw-pixel!)
(define render-texture primitive-render-texture!)
(define render-sprite primitive-render-sprite!)

(define-generic tick-func)
(define-method (tick-func (obj <game-object>))
  (slot-mod! obj 'counter 1+))

(define-generic draw-func)
(define-method (draw-func (obj <geo-object>)))

(define-generic collide-func)
(define-method (collide-func (obj-a <geo-object>)
                             (obj-b <geo-object>))
  #f)
