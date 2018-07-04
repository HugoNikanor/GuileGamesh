;;; A object which holds a list of scenes,
;;; and adds a keybind to go to the next scene.
;;; Really only for debugging

(define-module (objects scene-changer)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (engine)
  #:use-module (scene)
  #:use-module (event)
  #:use-module (event key)
  #:use-module (objects)
  #:export (<scene-changer>))

(define-class <scene-changer> (<game-object>)
  (name #:init-value "[SCENE CHANGER]")
  ;; This needs to be set, but it currently doesn't
  ;; signal an  error until it's to late
  (scene-list #:init-keyword #:list
	      #:getter scenes) 
  (next-key #:init-keyword #:key
	    #:getter next-key
	    #:init-value 'n))

(define-method (initialize (this <scene-changer>) args)
  (next-method)
  (for-each (cut add-event-listener! <keyboard-event> this <>)
	    (scenes this))
  (slot-set! this 'scene-list
	     (apply circular-list (scenes this))))

(define-method (event-do (this <scene-changer>)
			 (event <keyboard-event>))
  (next-method)
  (when (and (eqv? (slot-ref event 'type)
		   'SDL_KEYUP)
	     (= (slot-ref event 'scancode)
		(assv-ref scancodes (next-key this))))
	(step-scene-list! this)
	(set-current-scene! (car (scenes this)))))

(define (step-scene-list! this)
  (slot-set! this 'scene-list
	     (cdr (scenes this))))
