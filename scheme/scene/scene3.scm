(define-module (scene scene3)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (scene)
               #:use-module (objects sprite)

               #:use-module (arrowcontrol)

               #:use-module (util)
               #:use-module (vector)

               #:use-module (objects swap-sprite)
               
               #:export (scene3 s1 ss))

(with-new-scene scene3 "SCENE 3"
  ;;

  (define-class <ctrl-spr> (<sprite> <arrow-control>))

  (define-once s1 (make <ctrl-spr>))
  (define-once ss (make <swap-sprite>
                        #:name "Swap Sprite"
                        #:pos (v2 1 1)
                        #:file "assets/PathAndObjects_0.png"
                        #:size (v2 16 16)
                        #:amount (v2 10 10)
                        ))

  (do-once
    (register-draw-object! s1)
    (register-tick-object! s1)

    (register-draw-object! ss)
))
