(define-module (objects multi-sprite)
  #:use-module (oop goops)
  #:use-module (engine)
  #:use-module (vector)
  #:use-module (object)
  #:use-module (objects spritesheet)
  #:export (<multi-sprite>
            sprite-list
            size current-sprite
            access-sprite))


;;; A multi-sprite is like a swap-sprite, but with the difference that
;;; the different sprites are stored in different files.
(define-class <multi-sprite> (<geo-object>)
  (current-sprite #:init-value 0
                  #:init-keyword #:cur
                  #:accessor current-sprite)
  (file-list #:init-value '("assets/MissingTexture.jpg")
             #:init-keyword #:files
             #:allocation #:each-subclass)
  ;; This really should be a vector
  (sprite-list #:getter sprite-list
               #:allocation #:each-subclass
               #:init-value #f)
  ;; This currently holds just one value, should hold a pair 
  (size #:getter size #:allocation #:each-subclass))

(define-method (initialize (this <multi-sprite>) args)
  (next-method)
  (unless (sprite-list this)
    (slot-set! this 'sprite-list
               (map load-image (slot-ref this 'file-list)))
    (slot-set! this 'size
               (car (texture-size
                     (car (sprite-list this)))))))

(define-method (access-sprite (this <multi-sprite>)
                              current-sprite)
  "Access the current sprite, default implementation
Uses the current-sprite field for a numeric index, but
other implementations are free to do how they please."
  (list-ref (sprite-list this) current-sprite))

(define-method (draw-func (this <multi-sprite>))
  (next-method)
  (render-texture (access-sprite this (current-sprite this))
                  (size this)
                  '(0 0)
                  ;; TODO this is a hack since the render_texture
                  ;; primitive takes a list representing position
                  ;; on board, in number of tiles
                  (v2->list (m/ (pos this)
                                (v2 (size this)
                                    (size this))))
                  ))
