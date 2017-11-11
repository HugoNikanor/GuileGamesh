;;(define-module (vector))

(use-modules (oop goops))

(define-class <v2> ()
              (x #:accessor x #:init-keyword #:x)
              (y #:accessor y #:init-keyword #:y))

(define-method (+ (u <v2>)
                  (v <v2>))
               (make <v2>
                     #:x (+ (x u) 
                            (x v))
                     #:y (+ (y u)
                            (y v))))

(define-method (* (n <number>)
                  (v v2>))
               (make <v2>
                     #:x (* n (x v))
                     #:y (* n (y v))))

(define-method (- (u <v2>)
                  (v <v2>))
               (+ u (* -1 v)))
