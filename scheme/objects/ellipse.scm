(define-module
  (objects ellipse)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (objects) ;; <geo-object>
  #:use-module (engine)
  #:use-module (util)
  #:use-module (vector)

  ;; for current-scene in collide-func.
  ;; TODO probably move this
  #:use-module (scene) 

  #:use-module (collide)
  #:use-module (util)

  #:export (<ellipse>))

#|
 | ellipse should be split into two classes
 | the core class which is just the shape of
 | an ellispe. This is mostly to enable the
 | vector format to use it.
 |
 | The other part is the collision ellipse.
 |#

(define-class <ellipse> (<geo-object>)
              (r #:init-keyword #:r
                 #:init-value 1)
              (d #:init-keyword #:d
                 #:init-value 0)
              (color #:init-keyword #:color
                     #:init-value '(0 #xFF 0)))

(define-method (initialize (this <ellipse>) args)
               (register-collider! this)
               (next-method))

(define-method (draw-func (el <ellipse>))
  (apply set-color! (slot-ref el 'color))
  (draw-ellipse!
   (slot-ref el 'r)
   (slot-ref el 'd)
   (v2 (inexact->exact (floor (x (pos el))))
       (inexact->exact (floor (y (pos el))))))
  (next-method))

;;; TODO clean up this
(define-method (get-intersection-translation-vector
                (el1 <ellipse>)
                (el2 <ellipse>))
  (let ((s2 (helperfunction el1 el2))
        (s1 (helperfunction el2 el1)))
    (let ((v (pos el1))
          (u (pos el2)))
      (if (<= (abs (- v u))
             (+ (abs (- v s2))
                (abs (- u s1))))
          (* (- v u)
             (- (abs (- v u))
                (abs (- v s2))
                (abs (- u s1)))
             (/ 1 (abs (- v u))))
          (make <v2>)))))

;;; TODO clean up this
(define-method (helperfunction
                 (el1 <ellipse>)
                 (el2 <ellipse>))
               "Returns point"
               (let* ((help (x (- (pos el1)
                                  (pos el2))))
                      (theta (+ (if (> help 0) 0 pi)
                                (acos (/ (abs help)
                                         (abs (- (pos el1)
                                                 (pos el2)))))))
                      (r (slot-ref el1 'r))
                      (rx (/ r 2))
                      (ry (sqrt (- (square rx)
                                   (square (slot-ref el1 'd))))))
                 (+ (pos el1)
                    (make <v2>
                          #:x (* rx (cos theta))
                          #:y (* ry (sin theta))))))

(define-method (get-sub-point-helper func (el <ellipse>))
  (func (pos el)
        (make <v2> #:x (slot-ref el 'd))))


;;; TODO remove this
(define-method (colliding?
                 (el1 <ellipse>)
                 (el2 <ellipse>))
               (let ((togeled? #f))
                 (let ((el1_a (get-sub-point-helper + el1))
                       (el1_b (get-sub-point-helper - el1))
                       (el2_a (get-sub-point-helper + el2))
                       (el2_b (get-sub-point-helper - el2)))
                   (let ((v (- el1_a el2_a))
                         (u (- el1_b el2_b)))
                     ;; (‡ 0)
                     (when (> 1e-30 (abs (real-part (- (* (x v)
                                                          (y u))
                                                       (* (y v)
                                                          (x u))))))
                       (set! togeled? #t)
                       (set! v (- el1_b el2_a))
                       (set! u (- el1_a el2_b)))

                     (if togeled?
                       (begin
                         (set-color! #xFF 0 0) ; RED
                         (draw-line! el1_a el2_b)
                         (draw-line! el1_b el2_a))
                       (begin
                         (set-color! 0 #xFF 0) ; GREEN
                         (draw-line! el1_a el2_a)
                         (draw-line! el1_b el2_b)))
                       ;; TODO
                       (not (< (real-part (+ (slot-ref el1 'r)
                                             (slot-ref el2 'r)))
                               (real-part (+ (abs v)
                                             (abs u)))))))))

(define-method (collide-func (this <ellipse>))
  (for-each (lambda (that)
              (unless (eq? this that)
                (collide! this that)))
            (get-collide-list (current-scene))))


;;; TODO Actually figure out what can collide
;;; It will probably only be a rigid tile set
;;; and actors wich will all be ellipses
;; (define-generic collide!)
;;(define-method (collide <geo-object>))
(define-method (collide! (el1 <ellipse>)
                         (el2 <ellipse>))
  "Mutates el1"
  (slot-mod! el1 'pos
             (lambda (p)
               (- p (catch 'numerical-overflow
                           (lambda ()
                             (get-intersection-translation-vector el1 el2))
                           (lambda (symb . msg)
                             ;; Our algorithm doesn't work if the two
                             ;; ellipses are in the exact same position.
                             ;; We solve this by simply moving el1 one
                             ;; step to the right.
                             (make <v2> #:x 1)))))))
