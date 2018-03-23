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
                          ;; register-event-object!

                          register-keyboard-event!
                          register-mouse-motion-event!
                          register-mouse-button-event!

                          register-collider!
                          with-scene with-new-scene
                          ))


(define-class <scene> ()
              (name #:init-keyword #:name)
              ;; (event-list #:init-value '()
              ;;             #:getter get-event-list)

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
  (display "UNSUPORTED EVENT TYPE")
  (newline))

;; (define-method (dispatch-event (scene <scene>)
;;                                (event <object>))
;;   (display "NOT EVEN AN EVENT")
;;   (newline))

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

(define-method (dispatch-event (scene <scene>)
                               (event <mouse-button-event>))
  (for-with-false-break
   (cut event-do <> event)
   (get-mouse-button-event-list scene)))

;;; like a regular for-each, but returns early if the
;;; procedure returns  
;;; TODO this should be renamed and moved to some from of util lib
(define (for-with-false-break proc list)
  (let loop ((rem list))
    (if (or (null? rem)
            (not (proc (car rem))))
        #f
        (loop (cdr rem)))))

;; (define-method (add-event! (scene <scene>) item)
;;   (slot-set! scene 'event-list
;;              (cons item (slot-ref scene 'event-list))))

;; (define-method (add-tick! (scene <scene>) item)
;;   (slot-set! scene 'tick-list
;;              (cons item (slot-ref scene 'tick-list))))

;; (define-method (add-draw! (scene <scene>) item)
;;   (slot-set! scene 'draw-list
;;              (cons item (slot-ref scene 'draw-list))))

;; (define-method (add-collider! (scene <scene>) item)
;;   (slot-set! scene 'collision-list
;;              (cons item (slot-ref scene 'collision-list))))

(define cur-scene (make <scene>))
(define (current-scene) cur-scene)

(define (set-current-scene! scene)
  (set! cur-scene scene))

;; (add-tick! scene obj))
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
;; (define* (register-event-object! obj #:optional (scene (current-scene)))
;;          (add-event! scene obj))
;; (define* (register-keyboard-event-object! obj #:optional (scene (current-scene)))
;;   (add-keyboard-event! scene obj))

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
