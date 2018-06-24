(define-module (scene scene4)
  #:use-module (oop goops) ; make
  #:use-module (engine)
  #:use-module (scene)     ; with-new-scene
  #:use-module (util)      ; do-once
  #:use-module (vector)

  #:use-module (event)
  #:use-module (event mouse-btn)

  #:use-module (object) ; geo-object scene
  #:use-module (objects sprite)
  #:use-module (objects ss-inspector)
  #:use-module (objects ss-chooser)
  #:export (sheet-ins scene4))

(with-new-scene scene4 "SCENE 4"
  (define-once sheet-ins
    (make <ss-chooser>
      #:pos (v2 100 100)
      #:file "assets/PathAndObjects_0.png"
      #:amount (v2 16 16)
      #:size (v2 32 32)))

  (do-once
    (register-draw-object! sheet-ins)
    (add-event-listener! <mouse-button-event> sheet-ins)
))

