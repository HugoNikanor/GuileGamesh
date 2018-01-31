(define-module (event mouse-btn)
  #:use-module (engine)
  #:use-module (event)
  #:use-module (oop goops)
  #:use-module (vector)
  #:export (<mouse-btn-event>
	    mpos rpos
	    mouse-button

	    *mouse-left-btn*
	    *mouse-middle-btn*
	    *mouse-right-btn* 
	    ))

(define *mouse-left-btn*   1)
(define *mouse-middle-btn* 2)
(define *mouse-right-btn*  3)

(define-class <mouse-btn-event> (<event>)
  which state clicks
  (button #:getter mouse-button)
  x y
  (pos #:allocation #:virtual
       #:getter mpos
       #:slot-ref (lambda (o)
		    (v2 (slot-ref o 'x)
			(slot-ref o 'y)))
       #:slot-set! (lambda (o v)
		     (slot-set! o 'x (x v))
		     (slot-set! o 'y (y v))))

  (cobj #:init-form (make <geo-object>)
	#:accessor cobj)
  (rpos #:allocation #:virtual
	#:getter rpos
	#:slot-ref (lambda (o)
		     (- (mpos o)
			(pos (cobj o))))
	#:slot-set! (lambda (o v)
		     (slot-set! o 'pos (+ v (mpos (cobj o)))))))

(define-method (event-do (object <geo-object>)
			 (event <mouse-btn-event>))
  (set! (cobj event) object))

(define-method (fix-event-args (ev <mouse-btn-event>)
                               which button state clicks x y)
               (slot-set! ev 'which which)
               (slot-set! ev 'button button)
               (slot-set! ev 'state state)
               (slot-set! ev 'clicks clicks)
               (slot-set! ev 'x x)
               (slot-set! ev 'y y))
