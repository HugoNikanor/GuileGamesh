(define-module (objects chooser)
  #:use-module (oop goops)
  #:use-module (vector)
  #:use-module (engine)
  #:use-module (draw-help)

  #:use-module (event mouse-btn)
  #:use-module (oop goops describe)

  #:export (<chooser> ctile))

(define-class <chooser> (<geo-object>)
  (current-tile #:init-form (v2 0 0)
		#:accessor ctile))

(define-method (draw-func (this <chooser>))
  (next-method)
  (set-color #xFF #x7F #x7F #xA0)

  ;; This should really have a safeguard
  ;; ensuring that it's inside the sprite
  (draw-rect* #t 
	      (+ (pos this)
		 (m* (ctile this)
		     (slot-ref this 'single-size)))
	      (slot-ref this 'single-size)))

(define-method (event-do (this <chooser>)
                         (event <mouse-btn-event>))
  (display "---------- 1")
  ;;(next-method) ;; binds this to event, fixing rpos
  ;; Check that it was left btn, and btn released 
  (display "---------- 2")
  (describe event)
  (when (lclick? event)
    (let* ((tile-pos (floor (m/ (rpos event)
                                (slot-ref this 'single-size)))))
      (when (in-object? this (rpos event))
        (slot-set! this 'current-tile tile-pos)))))
