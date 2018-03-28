(define-module (scene scene5)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once

  #:use-module (vector)

  #:use-module (objects tileworld)
  #:export (scene5 world))

(with-new-scene scene5 "SCENE 5"
  ;;
  (define-once world
    (make <tileworld>
      #:file "assets/PathAndObjects_0.png"
      #:size (v2 32 32)
      #:amount (v2 16 16)))

  (do-once
    (register-draw-object! world)))
