#|
(define-module (scheme scene)
               #:use-module (oop goops)
               #:export (<scene>
                          current-scene
                          set-current-scene!
                          register-tick-object!
                          register-draw-object!
                          register-event-object!))
|#
(use-modules (oop goops))


(define-class <scene> ()
              (name #:init-keyword #:name)
              (event-list #:init-value '() #:getter get-event-list)
              (draw-list  #:init-value '() #:getter get-draw-list)
              (tick-list  #:init-value '() #:getter get-tick-list))

(define-method (add-event! (scene <scene>) item)
  (slot-set! scene 'event-list
             (cons item (slot-ref scene 'event-list))))

(define-method (add-tick! (scene <scene>) item)
  (slot-set! scene 'tick-list
             (cons item (slot-ref scene 'tick-list))))

(define-method (add-draw! (scene <scene>) item)
  (slot-set! scene 'draw-list
             (cons item (slot-ref scene 'draw-list))))

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
