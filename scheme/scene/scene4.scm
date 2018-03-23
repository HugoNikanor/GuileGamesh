;;(def-macro scene-modules
;;           `(begin
(define-module (scene scene4)
               #:use-module (oop goops) ;; make
               #:use-module (engine)
               #:use-module (scene) ;; with-new-scene
               #:use-module (util) ;; do-once
               #:use-module (vector)

               ;; #:use-module (scene common)

               #:use-module (event)
               #:use-module (event mouse-btn)

               #:use-module (objects sprite)
               #:use-module (objects ss-inspector)
               #:use-module (objects ss-chooser)
               #:export (sheet-ins scene4))

;;; Shouldn't this also be in the ss-chooser module?
(define-method (event-do (this <ss-chooser>)
                         (event <mouse-button-event>))
  (when (lclick? event)
    (let ((tile-pos (floor (m/ (pos event)
                               (single-size this)))))
      (set! (ctile this)
            tile-pos))))


(with-new-scene scene4 "SCENE 4"
  ;;

  (define-once sheet-ins
               (make <ss-chooser>
		 #:pos (v2 100 100)
                 #:file "assets/PathAndObjects_0.png"
                 #:amount (v2 16 16)
                 #:size (v2 32 32)))

  (do-once
    (register-draw-object! sheet-ins)
    (register-mouse-button-event! sheet-ins)
))

