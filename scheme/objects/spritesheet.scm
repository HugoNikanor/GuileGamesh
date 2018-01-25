(define-module (objects spritesheet)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (util)
               #:use-module (vector)
               #:export (<spritesheet>
                         <swap-sprite> move-cur))

(define-class <spritesheet> ()
              sprite
              (single-size #:init-value (v2 1 1)
                           #:init-keyword #:size)
              (amount #:init-value (v2 1 1)
                      #:init-keyword #:amount)
              (file #:init-keyword #:file))

(define-method (initialize (this <spritesheet>) args)
  (next-method)
  ;; Possibly check the sprite size against
  ;; amount and single-size. They don't need to match,
  ;; but should possibly be similar
  (slot-set!  this 'sprite
    (load-image
      (slot-ref* this 'file
        "assets/MissingTexture.jpg"))))

(define-class <swap-sprite> (<geo-object> <spritesheet>)
              (current-sprite #:init-value (v2 0 0)
                              #:init-keyword #:cur))

(define-generic move-cur)
(define-method (move-cur (this <swap-sprite>)
                         (dir <v2>))
               "Changes the current position by adding `dir',
                and then takeing that mod `amount'"
               (slot-set! this 'current-sprite
                          (modulo (+ (slot-ref this 'current-sprite)
                                     dir)
                                  (slot-ref this 'amount))))

(define (v2->list v2)
  (list (x v2) (y v2)))

(define-method (draw-func (this <swap-sprite>))
  (render-texture (slot-ref this 'sprite)
                  (car (v2->list (slot-ref this 'single-size)))
                  (v2->list (slot-ref this 'current-sprite))
                  (v2->list (pos this))))

