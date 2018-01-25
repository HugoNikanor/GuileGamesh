(define-module (scene scene3)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (scene)
               #:use-module (objects sprite)

               #:use-module (arrowcontrol)

               #:use-module (util)
               
               #:export (scene3 s1))

(with-new-scene scene3 "SCENE 3"
  ;;

  (define-class <ctrl-spr> (<sprite> <arrow-control>))

  (define-once s1 (make <ctrl-spr>))

  (do-once
    (register-draw-object! s1)
    (register-tick-object! s1)
))
