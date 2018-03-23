(define-module
  (event mouse-motion)
  #:use-module (event)
  #:use-module (oop goops)
  #:re-export (<mouse-motion-event> make-mouse-motion-event)
  #:export (
            ;; <mouse-motion-event>
            mousem-x mousem-y mouse-dx mouse-dy))

;; (define-class <mouse-motion-event> (<event>)
;;               which state
;;               (x #:getter mousem-x)
;;               (y #:getter mousem-y)
;;               (xrel #:getter mouse-dx)
;;               (yrel #:getter mouse-dy))

(define-method (fix-event-args (ev <mouse-motion-event>)
                               which state x y yrel xrel)
               (slot-set! ev 'which which)
               (slot-set! ev 'state state)
               (slot-set! ev 'x x)
               (slot-set! ev 'y y)
               (slot-set! ev 'xrel xrel)
               (slot-set! ev 'yrel yrel))
