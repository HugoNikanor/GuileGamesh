(define-module
  (event mouse-btn)
  #:use-module (event)
  #:use-module (oop goops)
  #:export (<mouse-btn-event>
             mouse-button mouseb-x mouseb-y))

(define-class <mouse-btn-event> (<event>) which state clicks
              (button #:getter mouse-button)
              (x #:getter mouseb-x)
              (y #:getter mouseb-y))
(define-method (fix-event-args (ev <mouse-btn-event>)
                               which button state clicks x y)
               (slot-set! ev 'which which)
               (slot-set! ev 'button button)
               (slot-set! ev 'state state)
               (slot-set! ev 'clicks clicks)
               (slot-set! ev 'x x)
               (slot-set! ev 'y y))
