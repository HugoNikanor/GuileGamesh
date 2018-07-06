;;; Module for common imports. Not all of these are needed in
;;; every module, but enough of them are, to warrant this file.

(read-set! keywords 'prefix)

(define-module (common)
  :use-module (oop goops)
  :use-module (oop goops describe)
  :use-module (engine)
  :use-module (objects)
  :use-module (vector)
  :use-module (util)
  :use-module (srfi srfi-26)
  #:re-export
  (make define-class define-method
        slot-ref slot-set! initialize
        describe
        draw-func tick-func collide-func
            set-window-size!
            draw-rect! set-color! draw-text! draw-line!
            draw-ellipse! load-image render-texture!
            render-sprite! texture-size
        <game-object> name counter parent
            <geo-object> pos in-object?
        <v2> x y v2 m* m/ v2->list
        apply-for-each cart-prod r slot-mod! square
            pi tau keyword-ref slot-ref* do-once push!
        cut))

;;; TODO figure out if there is a better way to max-rexport
