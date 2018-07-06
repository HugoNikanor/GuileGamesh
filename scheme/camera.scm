(define-module (camera)
  #:use-module (oop goops)
  #:use-module (oop goops describe)

  #:use-module (objects)
  #:use-module (vector)
  #:use-module (scene)
  #:export (<camera> make-camera)
  )

(define-class <camera> (<geo-object>)
  (size #:accessor size
        #:init-keyword #:size
        #:init-form (v2 100 100)))

(define (make-camera)
  (make <camera>))
