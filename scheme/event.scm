(define-module (event)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)

  ;; This is only imported for (define-generic pos)
  ;; That sexp should probably be placed somewhere better to minimize
  ;; imports
  ;; #:use-module (object)

  #:use-module (vector)

  #:use-module (pos)
  #:re-export (pos)
  #:export (
             ;; last-event
             <common-event>
             <keyboard-event>
             <mouse-motion-event>
             <mouse-button-event>
             ;; pos

             make-common-event
             make-keyboard-event
             make-mouse-motion-event
             make-mouse-button-event

             ;; handle-event
             ;; handle-curried

             event-do

             ;; pos

             ;; exported for c code?
             ;; fix-event-args
             ;; event-func
             ))

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

#| Provides:
 | - current-eventlist
 |#
;; (load-extension "./main" "expose_event")

#| last-event
 | Last event which was created
 | Should mainly be used for debugging
 |
 | Should really be of type Maybe Event
 |#
;; (define last-event '())

;;; obj -> event -> nothing
;; (define-generic handle-event)

;; (define ((handle-curried event) object)
;;   (handle-event object event))

;; (define-method (handle-event (obj <game-object>)
;;                              (event <common-event>))
;;   ;; Default method
;;   )

#| <event>
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


#| fix-event-args
 | This is a fix for rebinding the SDL structs into GOOPS
 | objects. There is a foreign object interface in Guile,
 | but I can't get it to work.
 |#
;; (define-generic fix-event-args)
;; (define-method (fix-event-args (_ <event>) rest))

#| event-do
 | Method to override if you want an event to do something
 | also requires registering it in the event-list
 |#
(define-generic event-do)

#| event-func
 | Takes an incomming event, populates it with useful
 | information, such as creation time.
 |
 | Then calls all objects registered to access events
 | (from event-list) with the event.
 |
 | TODO
 | This requires (event ...) to be into (current-module)
 | when called.
 |#
;; (define (event-func event)
;;   (set! last-event event)
;;   (let ((e (eval `((@ (oop goops) make) ,(car event))
;;                  (current-module))))
;;     (slot-set! e 'type (list-ref event 1))
;;     (slot-set! e 'timestamp (list-ref event 2))
;;     (slot-set! e 'window-id (list-ref event 3))
;;     (apply fix-event-args e (drop event 4))
;;     (for-each (lambda (obj)
;;                 (event-do obj e))
;;               (current-eventlist))))
