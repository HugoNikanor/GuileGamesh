(define-module (objects tileworld)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (engine)

  #:use-module (vector)
  #:use-module (util)

  #:use-module (objects)
  #:use-module (objects spritesheet)

  #:export (<tileworld> <tileworld-chooser>))

(define x-index-array (make-array #f 16 16))
(array-index-map! x-index-array (lambda (y x) x))

(define y-index-array (make-array #f 16 16))
(array-index-map! y-index-array (lambda (y x) y))

;; I kinda want to be able to morph any <ss-inpector> into
;; a <tileworld>. I have tried with <ss-chooser> -> <tileworld>,
;; But I only get missing slot errors.

;; size of world can't currently be changed after its creation
(define-class <tileworld> (<spritesheet> <geo-object>)
              ;; matrix of vectors pointing at tiles in spritesheet
              ;; World def is logical position
              world-def)

(define-method (initialize (this <tileworld>) args)
               (next-method)
               ;; typed arrays is a thing.
               ;; they might be faster
               (let ((arr
                       (apply
                         make-array #f
                         (v2->list (slot-ref this 'amount)))))
                 (slot-set! this 'world-def arr)
                 (array-index-map! arr
                   (lambda (y x)
                     (cond
                       ((= 0 x y) (v2 0 0))
                       ((= 0 x) (v2 0 1))
                       ((= 0 y) (v2 1 0))
                       ((= 0 (random 10)) (v2 2 4))
                       (else (v2 1 1)))))))

(define-method (in-object? (this <tileworld>)
                           (v <v2>))
  ;; Three might be a off by one error here
  (< (pos this)
     v
     (+ (pos this)
        (m* (slot-ref this 'single-size)
            (slot-ref this 'amount)))))

(define-method (draw-func (this <tileworld>))
  (array-for-each
    (lambda (tile x y)
      (render-texture! (slot-ref this 'sprite)
                       (single-size this)
                       tile
                       (+ (pos this)
                          (m* (single-size this)
                              (v2 x y)))))
    (slot-ref this 'world-def)
    x-index-array
    y-index-array))

(define-class <tileworld-chooser> (<tileworld>)
  (current-tile #:init-form (v2 0 0)
		#:accessor ctile))

(define-method (draw-func (this <tileworld-chooser>))
  (next-method)
  (set-color! #xFF #x7F #x7F #xA0)

  ;; This should really have a safeguard
  ;; ensuring that it's inside the sprite
  (draw-rect! #t 
	      (+ (pos this)
		 (m* (ctile this)
		     (slot-ref this 'single-size)))
	      (slot-ref this 'single-size)))



