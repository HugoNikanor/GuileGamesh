;; A module for drawing functions
;;
;; Mostly stuff that's much easier to do in scheme
;; code over C code. Such as alternative procedures
;; which works an <v2> instead of points.

(define-module (draw-help)
               #:use-module (engine)
               #:use-module (vector)
               #:export (draw-line*))

(define (draw-line* u v)
  "Draws a line between the two vectors"
  (draw-line (x u)
             (y u)
             (x v)
             (y v)))