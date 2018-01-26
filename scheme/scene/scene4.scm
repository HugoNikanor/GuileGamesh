;;(def-macro scene-modules
;;           `(begin
(define-module (scene scene4)
               #:use-module (oop goops) ;; make
               #:use-module (engine) ;; register-*-object!
               #:use-module (scene) ;; with-new-scene
               #:use-module (util) ;; do-once

               #:use-module (vector)
               #:use-module (objects ss-inspector)
               #:export (scene4))

(with-new-scene scene4 "SCENE 4"
  ;;

  (define-once sheet-ins
               (make <ss-inspector>
                     #:file "assets/PathAndObjects_0.png"))

  ;; Through running the program I saw that
  ;; these were the correct sizes for PathAndObjects_0.png
  (slot-set! sheet-ins 'amount (v2 16 16))
  (slot-set! sheet-ins 'single-size (v2 32 32))

  (do-once
    (register-draw-object! sheet-ins)))

