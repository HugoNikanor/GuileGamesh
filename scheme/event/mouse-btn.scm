(define-module (event mouse-btn)
  #:use-module (event)
  #:use-module (oop goops)
  ;; Something with this re-export doesn't work as expected
  #:re-export (<mouse-button-event> make-mouse-button-event pos)
  #:export (*mouse-left-btn*
	    *mouse-middle-btn*
	    *mouse-right-btn* 

	    lclick?
	    ))

(define *mouse-left-btn*   1)
(define *mouse-middle-btn* 2)
(define *mouse-right-btn*  3)

;; this should be a <mouse-btn-event>
(define (lclick? this)
  "Is the mouse event a left click"
  (and (= *mouse-left-btn* (slot-ref this 'button))
       (eqv? 'SDL_MOUSEBUTTONUP (slot-ref this 'type))))
