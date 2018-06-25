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
  (for-with-false-break
   (cut event-do <> (event-preprocess scene event))
   (hash-ref (event-listeners scene)
             (class-of event)
             '())))

;;; TODO it's an error to register an object for mouse events
;;; which isn't an <geo-object>. There should be some form of
;;; safeguards for this.

;;; TODO actually implement this
(define (z-order list)
  "Functions which returns the input list sorted by z coordinate."
  list)

;;; Some events (<mouse-button-event>) needs to reconfigure
;;; the event to match the scene in some ways. This allows
;;; that to be done.

(define-generic event-preprocess)

;; Default empty implementation
(define-method (event-preprocess (scene <scene>)
                                 (event <common-event>))
  event)

;;; mouse-button-events are special when it comes to
;;; dispatching them. This method loops through all objects
;;; currently registered to recieve mouse button events. It
;;; starts by filtering out all objects that doesn't contain
;;; the position of the event. It then sorts the objects in
;;; a z order and sends the mouse button event through the
;;; stack, with the coordinates of the event shifted to be
;;; local tho the event it's currently being sent to.
;;;     This method currently doesn't restore the position
;;; of the event once it's done. That can be implemented,
;;; but I won't since the event object is suposed to be
;;; thrown away after it's dispatched.

(define-method (event-preprocess (scene <scene>)
                                 (event <mouse-button-event>))
  (let ((original-position (pos event)))
    (call-with-prompt 'return
      (lambda ()
        (for-each (lambda (obj)
                    (set! (pos event)
                          (- original-position
                             (pos obj)))
                    (when (not (event-do obj event))
                      (abort-to-prompt 'return)))
                  (z-order
                   (filter (cut in-object? <> original-position)
                           (hash-ref (event-listeners scene)
                                     <mouse-button-event>)))))
      list))
  (next-method))

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
(define* (add-event-listener! type obj #:optional
                              (scene (current-scene)))
  (let ((h (event-listeners scene)))
    (hash-set! h type (cons obj (hash-ref h type '())))))
