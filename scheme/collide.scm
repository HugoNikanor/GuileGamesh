(define-module (collide)
  #:use-module (oop goops)
  #:use-module (object)
  #:export (colliding? collide!))

(define-generic colliding?)
(define-generic collide!)

(define-method (colliding? (a <geo-object>)
                           (b <geo-object>))
  #f)

;;; TODO
;;; Default implementation of collide!
