(define-module (objects ss-chooser)
               #:use-module (oop goops)
               #:use-module (oop goops describe)
               #:use-module (engine)
               ;;#:use-module (util)

               #:use-module (vector)
               #:use-module (draw-help)

               #:use-module (objects sprite)
               #:use-module (objects ss-inspector)

	       #:use-module (event mouse-btn)

	       #:use-module (objects chooser)
               #:export (<ss-chooser> #|ctile|#))

(define-class <ss-chooser> (<ss-inspector> <chooser>))

;; this isn't called
(define-method (event-do (this <ss-chooser>)
			 (event <mouse-btn-event>))
  (display "<ss-chooser> event do for <mouse-btn-event>\n"))

;; (define-class <ss-chooser> (<ss-inspector>)
;;               (current-tile #:init-form (v2 0 0)
;;                             #:accessor ctile))
;; 
;; 
;; (define-method (draw-func (this <ss-chooser>))
;;                (next-method)
;;                (set-color #xFF #x7F #x7F #xA0)
;; 
;;                ;; This should really have a safeguard
;;                ;; ensuring that it's inside the sprite
;;                (draw-rect*
;;                  #t 
;;                  (+ (pos this)
;;                     (m* (ctile this)
;;                         (single-size this)))
;;                  (single-size this)
;;                  ))
;;                     
;; ;; Most of this method shoud be generalized
;; ;; into macros and other functions.
;; (define-method (event-do (this <ss-chooser>)
;;                          (event <mouse-btn-event>))
;;   (next-method) ;; binds this to event, fixing rpos
;;   ;; Check that it was left btn, and btn released 
;;   (when (lclick? event)
;;     (let* ((tile-pos (floor (m/ (rpos event)
;;                                 (slot-ref this 'single-size)))))
;;       (when (in-object? this (rpos event))
;;         (slot-set! this 'current-tile tile-pos)))))
