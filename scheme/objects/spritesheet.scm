(define-module (objects spritesheet)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (util)
               #:use-module (vector)
               #:export (<spritesheet>
                         <swap-sprite> move-cur))

;; A spritesheets hold a refernce to an SDL_Texture,
;; along with how many sub images are in it, and how
;; large they are. It also knows which file it got the
;; data from.
;;     In the future I would like to also be able to
;; have spritesheets created from dynamic data
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

