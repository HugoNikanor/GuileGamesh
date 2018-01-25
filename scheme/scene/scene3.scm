(define-module (scene scene3)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (scene)
               #:use-module (objects sprite)
               
               #:export (scene3 s1))

;; TODO look into replacing *loaded* with gensymed
(define-macro (do-once . body)
  `(begin
     (define-once *loaded* #f)
     (unless *loaded*
       (set! *loaded* #t)
       ,@body)))

(define-once scene3 (make <scene> #:name "SCENE 3"))

(with-scene scene3

(define-once s1 (make <sprite>))

(do-once
  (register-draw-object! s1))
)
