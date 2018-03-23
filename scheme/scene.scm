(define-module (scene)
               #:use-module (oop goops)
               #:use-module (srfi srfi-26)
               #:use-module (event)
               #:export (<scene>
                         dispatch-event
                         current-scene
                         set-current-scene!
                         get-event-list
                         get-draw-list
                         get-tick-list
                         get-collide-list
                         register-tick-object!
                         register-draw-object!

                         register-keyboard-event!
                         register-mouse-motion-event!
                         register-mouse-button-event!

                         register-collider!
                         with-scene with-new-scene
                         ))


(define-class <scene> ()
              (name #:init-keyword #:name)

              (mouse-motion-event-list
               #:init-value '()
               #:getter get-mouse-motion-event-list)
              (mouse-button-event-list
               #:init-value '()
               #:getter get-mouse-button-event-list)
              (keyboard-event-list
               #:init-value '()
               #:getter get-keyboard-event-list)

              (draw-list  #:init-value '()
                          #:getter get-draw-list)
              (tick-list  #:init-value '()
                          #:getter get-tick-list)
              (collision-list #:init-value '()
                              #:getter get-collide-list))

(define-generic dispatch-event)

(define-method (dispatch-event (scene <scene>)
                               (event <common-event>))
  ;; This is for all unsupported event types
  )

(define-method (dispatch-event (scene <scene>)
                               (event <keyboard-event>))
  (for-with-false-break
   (cut event-do <> event)
   (get-keyboard-event-list scene)))

(define-method (dispatch-event (scene <scene>)
                               (event <mouse-motion-event>))
  (for-with-false-break
   (cut event-do <> event)
   (get-mouse-motion-event-list scene)))

;;; TODO it's an error to register an object for mouse events
;;; which isn't an <geo-object>. There should be some form of
;;; safeguards for this.

;;; TODO actually implement this
(define (z-order list)
  "Functions which returns the input list sorted by z coordinate."
  list)

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
(define-method (dispatch-event (scene <scene>)
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
                           (get-mouse-button-event-list scene)))))
      list)))

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

(define (set-current-scene! scene)
  (set! cur-scene scene))

(define* (register-tick-object! obj #:optional (scene (current-scene)))
  (slot-set! scene 'tick-list
             (cons obj (slot-ref scene 'tick-list))))
(define* (register-draw-object! obj #:optional (scene (current-scene)))
  (slot-set! scene 'draw-list
             (cons obj (slot-ref scene 'draw-list))))
(define* (register-collider! obj #:optional (scene (current-scene)))
  (slot-set! scene 'collision-list
             (cons obj (slot-ref scene 'collision-list))))
(define* (register-keyboard-event! obj #:optional (scene (current-scene)))
  (slot-set! scene 'keyboard-event-list
             (cons obj (slot-ref scene 'keyboard-event-list))))
(define* (register-mouse-motion-event! obj #:optional (scene (current-scene)))
  (slot-set! scene 'mouse-motion-event-list
             (cons obj (slot-ref scene 'mouse-motion-event-list))))
(define* (register-mouse-button-event! obj #:optional (scene (current-scene)))
  (slot-set! scene 'mouse-button-event-list
             (cons obj (slot-ref scene 'mouse-button-event-list))))

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
