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

;; Most of this method shoud be generalized
;; into macros and other functions.
(define-method (event-do (this <ss-chooser>)
                         (event <mouse-btn-event>))
  ;;
  ;; Check that it was left btn, and btn released 
  (when (and (= *mouse-left-btn* (slot-ref event 'button))
             (eqv? 'SDL_MOUSEBUTTONUP (slot-ref event 'type)))

    (let* ((click (v2 (mouseb-x event) (mouseb-y event)))
           (rel-coord (- click (slot-ref this 'pos)))
           (tile-pos (floor (m/ rel-coord
                                (slot-ref this 'single-size)))))

      ;; The thing about getting the relative position,
      ;; and ensuring that the event is within should
      ;; be done somewhere else
      (when (< (v2) rel-coord (slot-ref this 'size))
        (slot-set! this 'current-tile tile-pos)))))

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
    (register-event-object! sheet-ins)
))

