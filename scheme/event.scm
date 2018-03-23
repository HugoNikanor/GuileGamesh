(define-module (event)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)

  #:use-module (vector)

  #:use-module (pos)
  #:re-export (pos)
  #:export (<common-event>
             <keyboard-event>
             <mouse-motion-event>
             <mouse-button-event>

             make-common-event
             make-keyboard-event
             make-mouse-motion-event
             make-mouse-button-event

             event-do))

;;; Why does the helper functions have to be before the actual
;;; functions here? That usually isn't the case.

(define-macro (create-make-func type . args)
  `(let ((ev (make ,type)))
     ,@ (map (lambda (t)
               `(slot-set! ev (quote ,t) ,t))
             args)
        ev))

(define-macro (define-event-make-func name type . args)
  `(define (,name ,@args)
     (create-make-func ,type ,@args)))

(define (v-from-obj obj)
  (v2 (slot-ref obj 'x)
      (slot-ref obj 'y)))

(define (obj-v-save obj vec)
  (slot-set! obj 'x (x vec))
  (slot-set! obj 'y (y vec)))

#| <common-event>
 | Event class represents SDL events
 |#
(define-class <common-event> ()
              type timestamp)

(define-event-make-func make-common-event <common-event>)

(define-class <keyboard-event> (<common-event>)
  ;; Type \in { SDL_KEYDOWN, SDL_KEYUP }
  window-id
  state ; \in { SDL_PRESSED, SDL_RELEASED }
  repeat

  ;; These three are originally in the keysym field
  scancode sym mod
  )

(define-event-make-func make-keyboard-event <keyboard-event>
  type timestamp window-id state repeat scancode sym mod)

(define-class <mouse-motion-event> (<common-event>)
  ;; type \in { SDL_MOUSEMOTION }
  window-id
  which ; which mouse?
  state
  x y
  xrel yrel
  )

(define-event-make-func make-mouse-motion-event <mouse-motion-event>
  type timestamp window-id which state x y xrel yrel)

(define-class <mouse-button-event> (<common-event>)
  ;; type \in { SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP }
  window-id
  which
  button
  state
  clicks
  x y

  (pos #:accessor pos
       #:allocation #:virtual
       #:slot-ref v-from-obj
       #:slot-set! obj-v-save)
  )

(define-event-make-func make-mouse-button-event <mouse-button-event>
  type timestamp window-id which button state clicks x y)


#| event-do
 | Method to override if you want an event to do something
 | also requires registering it in the event-list
 |#
(define-generic event-do)
