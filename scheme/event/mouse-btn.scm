(define-module (event mouse-btn)
  #:use-module (event)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)
  ;; Something with this re-export doesn't work as expected
  #:re-export (<mouse-button-event> make-mouse-button-event pos)
  #:export (*mouse-left-btn*
	    *mouse-middle-btn*
	    *mouse-right-btn* 

	    lclick? rclick?
	    ))

(define *mouse-left-btn*   1)
(define *mouse-middle-btn* 2)
(define *mouse-right-btn*  3)

;; this should be a <mouse-btn-event>

(define ((click? btn) event)
  (and (= btn (slot-ref event 'button))
       (eqv? 'SDL_MOUSEBUTTONUP (slot-ref event 'type))))

(define lclick? (click? *mouse-left-btn*))
(define rclick? (click? *mouse-right-btn*))
