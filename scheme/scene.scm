(define-module (scene)
               #:use-module (oop goops)
               #:export (<scene>
                          current-scene
                          set-current-scene!
                          get-event-list
                          get-draw-list
                          get-tick-list
                          get-colliders
                          register-tick-object!
                          register-draw-object!
                          register-event-object!
                          register-collider!
                          with-scene
                          ))


(define-class <scene> ()
              (name #:init-keyword #:name)
              (event-list #:init-value '()
                          #:getter get-event-list)
              (draw-list  #:init-value '()
                          #:getter get-draw-list)
              (tick-list  #:init-value '()
                          #:getter get-tick-list)
              (collision-list #:init-value '()
                              #:getter get-colliders))

(define-method (add-event! (scene <scene>) item)
  (slot-set! scene 'event-list
             (cons item (slot-ref scene 'event-list))))

(define-method (add-tick! (scene <scene>) item)
  (slot-set! scene 'tick-list
             (cons item (slot-ref scene 'tick-list))))

(define-method (add-draw! (scene <scene>) item)
  (slot-set! scene 'draw-list
             (cons item (slot-ref scene 'draw-list))))

(define-method (add-collider! (scene <scene>) item)
  (slot-set! scene 'collision-list
             (cons item (slot-ref scene 'collision-list))))

(define cur-scene (make <scene>))
(define (current-scene) cur-scene)

(define (set-current-scene! scene)
  (set! cur-scene scene))

(define* (register-tick-object! obj #:optional (scene (current-scene)))
         (add-tick! scene obj))
(define* (register-draw-object! obj #:optional (scene (current-scene)))
         (add-draw! scene obj))
(define* (register-event-object! obj #:optional (scene (current-scene)))
         (add-event! scene obj))
(define* (register-collider! obj #:optional (scene (current-scene)))
         (add-collider! scene obj))

(define-macro (with-scene scene . exprs)
  "call <exprs> with (current-scene) set to return <scene>
   return unspecified"
  (let ((orig-scene (gensym)))
    `(begin
       (define ,orig-scene current-scene)
       (set! current-scene (lambda () ,scene))
       ,@exprs
       (set! current-scene ,orig-scene))))
