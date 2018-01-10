(define-module
  (collide)
  #:use-module (oop goops)

  #:export (colliding? collide!))

(define-generic colliding?)
(define-generic collide!)
