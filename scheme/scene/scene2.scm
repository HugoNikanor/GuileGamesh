(define-module (scene scene2)
               #:use-module (oop goops)
               #:use-module (engine) ;; tick-func
               #:use-module (scene)
               #:use-module (vector)
               #:use-module (objects ellipse)
               ;;#:use-module (objects vecloader)

               #:use-module (collide)

               #:use-module (util)
               #:use-module (arrowcontrol)
               #:export (eel pel
                             ))

#|
(define coll-text (make <text-obj>
                        #:pos (make <v2> #:y 5 #:x 5)
                        #:str "U"
                        #:update (lambda (_)
                                   (if (colliding? pel eel)
                                     "T" "F"))))

(register-draw-object! coll-text)
(register-tick-object! coll-text)
|#

(define-class <ctrl-el> (<ellipse> <arrow-control>))
(define-once pel
  (make <ctrl-el>
        #:name "[ELLIPSE 1]"
        #:pos (make <v2> #:x 40 #:y 40)
        #:r 100
        #:d 30))

(define-once eel
  (make <ellipse>
        #:name "[ELLIPSE 2]"
        #:pos (make <v2> #:x 120 #:y 120)
        #:color '(#xFF 0 0)
        #:r 100
        #:d 30))

;; (define-once figure
;;   (parse (open-input-file "assets/obj.sxml")))

(define f (open-input-file "assets/obj.sxml"))

(define-once *loaded* #f)

;; TODO create a load-once macro
(unless *loaded*
  (set! *loaded* #t)

  ;(register-draw-object! pel)
  ;;(register-event-object! pel)
  (register-tick-object! pel)

  ;(register-draw-object! eel)
  ;;(register-event-object! eel)
  ;;(register-tick-object! eel)
  )

(define-method (tick-func (obj <ctrl-el>))
               (collide! obj eel))

;;               (let ((other eel))
;;                 (if (not (eq? obj other))
;;                   (collide! obj other))))
