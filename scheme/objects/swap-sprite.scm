(define-module (objects swap-sprite)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (vector)
               #:use-module (objects spritesheet)
               #:export (<swap-sprite> move-cur))


;; A swap-sprite is just a simple sprite that instead of holding
;; it's own sprite data instead extends a spritesheet. It then
;; uses the slot `current-sprite' to choose which part of the
;; spritesheet is the current thing to render.
(define-class <swap-sprite> (<geo-object> <spritesheet>)
              (current-sprite #:init-value (v2 0 0)
                              #:init-keyword #:cur))

;; Changes the current sprite by adding `dir',
;; and then ensuring it's within the bounds set
;; by `amount' in <spritesheet>.
;;     Mostly for debugging, since actual stuff
;; are stricter on what they can show
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

