(define-module (scene)
               #:use-module (oop goops)
               #:use-module (srfi srfi-26)
               #:use-module (event)
               #:use-module (object)
               #:export (<scene>
                         dispatch-event
                         current-scene
                         set-current-scene!
                         get-draw-list
                         get-tick-list
                         get-collide-list
                         register-tick-object!
                         register-draw-object!

                         add-event-listener!
                         event-listeners
                         listeners-of-type

                         register-collider!
                         with-scene with-new-scene

                         <scene-changed-event>
                         <scene-changed-in-event>
                         <scene-changed-out-event>
                         new-scene old-scene
                         )
               #:re-export (event-do name))


(define-class <scene> ()
  (name #:init-keyword #:name
        #:getter name)

  (event-listeners
   #:init-form (make-hash-table)
   #:getter event-listeners)

  (draw-list  #:init-value '()
              #:getter get-draw-list)
  (tick-list  #:init-value '()
              #:getter get-tick-list)
  (collision-list #:init-value '()
                  #:getter get-collide-list))

;;; TODO a better system for keeping track of past events should
;;; probably be put in place. Should only be used for debugging.
(define last-event #f)

(define (dispatch-event scene event)
  (set! last-event event)
  (for-each
   (lambda (object)
     (when (event-guard scene object event)
       (event-do object event)))
   (listeners-of-type scene (class-of event))))

(define (listeners-of-type scene type)
  "Returns all event listeners of specified type in scene.
Type should be a class."
  (hashq-ref (event-listeners scene)
             (class-name type)
             '()))

;;; TODO it's an error to register an object for mouse events
;;; which isn't an <geo-object>. There should be some form of
;;; safeguards for this.

;;; Event guards can be registered. This method should return
;;; #f if the object shouldn't recieve the event, and #t if it
;;; should. Currently used to only forward mouse-button-events
;;; to objects which are within the click.

(define-generic event-guard)

(define-method (event-guard (scene <scene>)
                            (object <object>)
                            (event <common-event>))
  #t)

(define-method (event-guard (scene <scene>)
                            (object <geo-object>)
                            (event <mouse-button-event>))
  (in-object? object (pos event)))

;;; like a regular for-each, but returns early if the
;;; procedure returns  
;;; TODO this should be renamed and moved to some from of util lib
(define (for-with-false-break proc list)
  (let loop ((rem list))
    (if (or (null? rem)
            (not (proc (car rem))))
        #f
        (loop (cdr rem)))))

(define cur-scene (make <scene>))
(define (current-scene) cur-scene)

(define-class <scene-changed-event> (<common-event>)
  (new-scene #:getter new-scene
             #:init-keyword #:new)
  (old-scene #:getter old-scene
             #:init-keyword #:old))

(define-class <scene-changed-in-event> (<scene-changed-event>))
(define-class <scene-changed-out-event> (<scene-changed-event>))

;; Sets the current scene to 'scene,
;; sends an event to the shifted in and out scene
;; about what has happened.
(define (set-current-scene! scene)
  (let ((ev (make <scene-changed-event>
              #:old cur-scene
              #:new scene)))
    (change-class ev <scene-changed-out-event>)
    (event-do cur-scene ev)
    (set! cur-scene scene)
    (change-class ev <scene-changed-in-event>)
    (event-do cur-scene ev)))

(define-method (event-do (this <scene>)
                         (event <scene-changed-event>)))

(define* (register-tick-object! obj #:optional (scene (current-scene)))
  (slot-set! scene 'tick-list
             (cons obj (slot-ref scene 'tick-list))))
(define* (register-draw-object! obj #:optional (scene (current-scene)))
  (slot-set! scene 'draw-list
             (cons obj (slot-ref scene 'draw-list))))
(define* (register-collider! obj #:optional (scene (current-scene)))
  (slot-set! scene 'collision-list
             (cons obj (slot-ref scene 'collision-list))))

(define-macro (with-scene scene . exprs)
  "call <exprs> with (current-scene) set to return <scene>
   return unspecified"
  (let ((orig-scene (gensym)))
    `(begin
       (define ,orig-scene current-scene)
       (set! current-scene (lambda () ,scene))
       ,@exprs
       (set! current-scene ,orig-scene))))

(define-macro (with-new-scene symb name . exprs)
  "Creates a new scene bound to `symb' with `name'
   And then call the rest with that scene as (current-scene)"
  `(begin
     (define-once ,symb (make <scene> #:name ,name))
     (with-scene ,symb ,@exprs)))


;; Currently all event listeners are stored in linked lists
;; inside a hash table. The hash table stays, but the linked
;; lists can possibly be replaced with other iteratable
;; datastructures.
;;
;; Type is supposed to be a class, but the value stored
;; is the name of the class, as a symbol.
(define* (add-event-listener! type obj #:optional
                              (scene (current-scene)))
  (let ((h (event-listeners scene)))
    (hashq-set! h (class-name type)
                (cons obj (listeners-of-type scene type)))))
