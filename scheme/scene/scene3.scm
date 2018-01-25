(define-module (scene scene3)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (scene)
               #:use-module (objects sprite)

               #:use-module (util)
               
               #:export (scene3 s1))

(with-new-scene scene3 "SCENE 3"

  (define-once s1 (make <sprite>))

  (do-once
    (register-draw-object! s1)
))
