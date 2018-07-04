(define-module (objects ss-chooser)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (engine)

  #:use-module (vector)
  #:use-module (draw-help)

  #:use-module (event)
  #:use-module (event mouse-btn)

  #:use-module (objects)
  #:use-module (objects sprite)
  #:use-module (objects ss-inspector)
  #:export (<ss-chooser> ctile))

(define-class <ss-chooser> (<ss-inspector>)
  (current-tile #:init-form (v2 0 0)
                #:accessor ctile))

(define-method (event-do (this <ss-chooser>)
                         (event <mouse-button-event>))
  (when (lclick? event)
    (let ((tile-pos (floor (m/ (pos event)
                               (single-size this)))))
      (set! (ctile this)
            tile-pos))))

(define-method (draw-func (this <ss-chooser>))
  (next-method)
  (set-color #xFF #x7F #x7F #xA0)
  (draw-rect*
   #t 
   (+ (pos this)
      (m* (ctile this)
          (single-size this)))
   (single-size this)))
                    
