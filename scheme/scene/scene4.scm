;;(def-macro scene-modules
;;           `(begin
(define-module (scene scene4)
               #:use-module (oop goops) ;; make
               #:use-module (engine) ;; register-*-object!
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

(define-generic in-object?)
(define-method
  (in-object? (this <sprite>)
              (pos <v2>))
  (< (v2) pos (slot-ref this 'size)))

(define-method
  (lclick? (this <mouse-btn-event>))
  "Is the mouse event a left click"
  (and (= *mouse-left-btn* (slot-ref this 'button))
       (eqv? 'SDL_MOUSEBUTTONUP (slot-ref this 'type))))

;; Most of this method shoud be generalized
;; into macros and other functions.
(define-method (event-do (this <ss-chooser>)
                         (event <mouse-btn-event>))
  (next-method) ;; binds this to event, fixing rpos
  ;; Check that it was left btn, and btn released 
  (when (lclick? event)
    (let* ((tile-pos (floor (m/ (rpos event)
                                (slot-ref this 'single-size)))))
      (when (in-object? this (rpos event))
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

