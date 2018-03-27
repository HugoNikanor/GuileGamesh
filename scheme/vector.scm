(define-module (vector)
  #:use-module (oop goops)
  #:export (<v2> x y v2 m* m/ v2->list))

(define-class <v2> ()
  (x #:init-value 0
     #:accessor x
     #:init-keyword #:x)
  (y #:init-value 0
     #:accessor y
     #:init-keyword #:y))

(define* (v2 #:optional
             (x 0)
             (y 0))
  (make <v2> #:x x #:y y))

;;; TODO remove this extra functionality and use
;;; the v2 function instead.
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

(define (v2->list v)
  (list (x v)
        (y v)))

(define-method (< (u <v2>)
                  (v <v2>))
  (and (< (x u)
          (x v))
       (< (y u)
          (y v))))
               

(define-method (m* (u <v2>)
                   (v <v2>))
  "Element-wise product"
  (v2 (* (x u)
         (x v))
      (* (y u)
         (y v))))

(define-method (m/ (u <v2>)
                   (v <v2>))
  "Element-wise division"
  (v2 (/ (x u)
         (x v))
      (/ (y u)
         (y v))))

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

(define-method (modulo (v <v2>)
                       (u <v2>))
  (v2 (modulo (x v)
              (x u))
      (modulo (y v)
              (y u))))

(define-method (floor (v <v2>))
  (v2 (floor (x v))
      (floor (y v))))

(define-method (map proc (v <v2>))
  (v2 (proc (x v))
      (proc (y v))))
