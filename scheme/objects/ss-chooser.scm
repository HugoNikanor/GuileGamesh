(define-module (objects ss-chooser)
               #:use-module (oop goops)
               #:use-module (oop goops describe)
               #:use-module (engine)
               ;;#:use-module (util)

               #:use-module (vector)
               #:use-module (draw-help)

               #:use-module (object)
               #:use-module (objects sprite)
               #:use-module (objects ss-inspector)
               #:export (<ss-chooser> ctile))

(define-class <ss-chooser> (<ss-inspector>)
              (current-tile #:init-form (v2 0 0)
                            #:accessor ctile))


(define-method (draw-func (this <ss-chooser>))
               (next-method)
               (set-color #xFF #x7F #x7F #xA0)

               ;; This should really have a safeguard
               ;; ensuring that it's inside the sprite
               (draw-rect*
                 #t 
                 (+ (pos this)
                    (m* (ctile this)
                        (single-size this)))
                 (single-size this)
                 ))
                    
