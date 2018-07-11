;;; This is where the C code is sourced,
;;; and also where the main procedures get
;;; their declarations.
;;; I belive this is also where the graphical
;;; window is spawned, but I'm not sure

(define-module (engine)
  #:use-module (oop goops)
  #:use-module (util)
  #:use-module (objects)
  #:use-module (vector)
  #:use-module (scene)

  #:export (ready!

            draw-rect! set-color! draw-text!
            draw-line! draw-ellipse!
            render-texture! render-sprite!

            texture-size load-image

            draw-func tick-func collide-func

            set-window-size!
            ))

(load-extension "./main" "init_functions")

#; (define set-color set-color!)

#; (define draw-rect primitive-draw-rect!)
(define* (draw-rect! fill? top-left size
                     #:optional (camera (current-camera)))
  (let ((local-tl (- top-left (pos camera))))
    (primitive-draw-rect! fill?
               (x local-tl) (y local-tl)
               (x size) (y size))))

(define draw-text primitive-draw-text!)

;;; TODO support for different fonts
(define* (draw-text! str position
                     #:optional (camera (current-camera)))
  (let ((lp (- position (pos camera))))
    (primitive-draw-text! str (x lp) (y lp))))

#; (define draw-line primitive-draw-line!)
(define* (draw-line! from to #:optional (camera (current-camera)))
  (let ((lfrom (- from (pos camera)))
        (lto (- to (pos camera))))
    (primitive-draw-line! (x lfrom) (y lfrom)
                          (x lto) (y lto))))

#; (define draw-ellipse primitive-draw-ellipse!)

(define* (draw-ellipse! radius distance center
                        #:optional (camera (current-camera)))
  "TODO figure out what radius is in this context
distance is distance between center and either focus
center is a v2"
  (let ((lc (- center (pos camera))))
    (primitive-draw-ellipse! radius (x lc) (y lc) distance)))

#; (define draw-pixel primitive-draw-pixel!)

(define* (draw-pixel! pixel #:optional (camera (current-camera)))
  (let ((l (- pixel (pos camera))))
    (primitive-draw-pixel! (x l) (y l))))

#; (define render-texture primitive-render-texture!)

(define* (render-texture! image tile-size sheet-pos world-pos
                          #:optional (camera (current-camera)))
  "Procedure for drawing a tile from a spritesheet.
- Image is an image ptr
- tile-size is how large each tile is, taken as a v2.
- sheet-pos is the position (in tiles) of the desired sprite
- board pos is the location in the world to draw the sprite"
  (let ((p (- world-pos (pos camera))))
   (primitive-render-texture!
    image
    (x p) (y p)
    (x tile-size) (y tile-size)
    (x sheet-pos) (y sheet-pos))))

#; (define render-sprite primitive-render-sprite!)

(define* (render-sprite! image position
                         #:optional (camera (current-camera)))
  "Render a single sprite"
  (let ((p (- position (pos camera))))
    (primitive-render-sprite! image (v2->list p))))

(define-generic tick-func)
(define-method (tick-func (obj <game-object>))
  (slot-mod! obj 'counter 1+))

(define-generic draw-func)
(define-method (draw-func (obj <geo-object>)))

(define-generic collide-func)
(define-method (collide-func (obj-a <geo-object>)
                             (obj-b <geo-object>))
  #f)
