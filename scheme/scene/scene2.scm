(define-module (scene scene2)
  #:use-module (oop goops)
  #:use-module (engine) ;; tick-func
  #:use-module (scene)
  #:use-module (vector)
  #:use-module (objects ellipse)
  #:use-module (objects character)

  #:use-module (collide)

  #:use-module (util)
  #:use-module (arrowcontrol)
  #:export (eel pel test-character
                scene2
                ))

#| TODO
 | Quite a lot of stuff here should be out-macro'd
 |
 | - defining the scene should possibly be implicit
 | - everything below being in the scene can be prettier
 | - Class declarations should possibly not be in a scene
 | - Instead have some nice way of handling classes connected to scenes
 | - Propper way of only loading some stuff the first time the
 |   scene is loaded.
 | - Clear way to mark stuff that should be reloaded
 |#

#| Secound TODO
 | I don't want to recreate the objects in the
 | scene on reload. But I want to update them
 | to contain everything new. This is possibly
 | done automaticly. Otherwise something like
 | a class replace should suffice.
 |#

(with-new-scene
  scene2 "SCENE 2"
  (define-class <ctrl-el> (<ellipse> <arrow-control>))

  (define-once pel
    (make <ctrl-el>
          #:name "[ELLIPSE 1]"
          #:pos (make <v2> #:x 40 #:y 40)
          #:r 100
          #:d 30))

  (define-once eel
    (make <ellipse>
          #:name "[ELLIPSE 2]"
          #:pos (make <v2> #:x 120 #:y 120)
          #:color '(#xFF 0 0)
          #:r 100
          #:d 30))

  (define f (open-input-file "assets/obj.sxml"))
  (define test-character
    (make <character>
          #:file (open-input-file "assets/obj.sxml")
          #:pos (make <v2> #:x 200 #:y 200)))
  
  (do-once
    (register-tick-object! pel)
    (register-draw-object! pel)

    (register-draw-object! eel)

    (register-draw-object! test-character)

))
