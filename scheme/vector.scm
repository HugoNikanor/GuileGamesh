(define-module (vector)
               #:use-module (oop goops)
               #:export (<v2> x y))

(define-class <v2> ()
              (x #:init-value 0 #:accessor x #:init-keyword #:x)
              (y #:init-value 0 #:accessor y #:init-keyword #:y))

(define-method (initialize (obj <v2>) initargs)
               "Allows (make <v2> '(1 2)) as an alternative to
               (make <v2> #:x 1 #:y 2)"
               (if (and (not (null? initargs))
                        (list? (car initargs)))
                 (let ((x (list-ref (car initargs) 0))
                       (y (list-ref (car initargs) 1)))
                   (slot-set! obj 'x x)
                   (slot-set! obj 'y y))
                 (next-method)))


(define-method (+ (u <v2>)
                  (v <v2>))
               (make <v2>
                     #:x (+ (x u)
                            (x v))
                     #:y (+ (y u)
                            (y v))))

(define-method (* (n <number>)
                  (v <v2>))
               (make <v2>
                     #:x (* n (x v))
                     #:y (* n (y v))))

(define-method (* (v <v2>)
                  (n <number>))
               (* n v))

(define-method (- (u <v2>)
                  (v <v2>))
               (+ u (* -1 v)))

(define (square x)
  (* x x))

(define-method (abs (v <v2>))
               (sqrt (+ (square (x v))
                        (square (y v)))))

