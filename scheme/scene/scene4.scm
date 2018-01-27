;;(def-macro scene-modules
;;           `(begin
(define-module (scene scene4)
               #:use-module (oop goops) ;; make
               #:use-module (engine) ;; register-*-object!
               #:use-module (scene) ;; with-new-scene
               #:use-module (util) ;; do-once

               #:use-module (event)
               #:use-module (event mouse-btn)

               #:use-module (vector)
               #:use-module (objects ss-inspector)
               #:use-module (objects ss-chooser)
               #:export (sheet-ins scene4))

;;(define-method (event-do (this <ss-chooser>) (event <event>)))
(define-method (event-do (this <ss-chooser>)
                         (event <mouse-btn-event>))
  ;;
  (let ((click (v2 (mouseb-x event) (mouseb-y event))))
    (slot-set! this 'current-tile
      (floor (m/ (- click (slot-ref this 'pos))
                (slot-ref this 'single-size))))))

(with-new-scene scene4 "SCENE 4"
  ;;

  (define-once sheet-ins
               (make <ss-chooser>
                 #:file "assets/PathAndObjects_0.png"
                 #:amount (v2 16 16)
                 #:size (v2 32 32)))

  (do-once
    (register-draw-object! sheet-ins)
    (register-event-object! sheet-ins)
))

