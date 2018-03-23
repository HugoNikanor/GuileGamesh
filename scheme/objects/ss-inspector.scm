;;; SpriteSheet Inspector
;;; An object for extracting tiles from tile-sheets
;;; 
;;; Currently only works for rectangular tiles

(define-module (objects ss-inspector)
               #:use-module (oop goops)
               #:use-module (engine)
               #:use-module (util)

               #:use-module (srfi srfi-26)

               #:use-module (vector)
               #:use-module (draw-help)
               #:use-module (object)
               #:use-module (objects spritesheet)
               #:use-module (objects sprite)

               #:export (<ss-inspector> single-size))

;; This is designed to be able to be changed into a spritesheet

(define-class <ss-inspector> (<sprite>)
              (single-size #:init-value (v2 1 1)
                           #:init-keyword #:size
                           #:getter single-size)
              (amount #:init-value (v2 1 1)
                      #:init-keyword #:amount
                      #:getter amount)
              (color #:init-keyword #:color
                     #:init-value '(#xFF 0 0))
              )

;;(define-method (initialize (this <ss-inspector>) args)
;;               (next-method))

;; draws all lines in top left of tile
(define-method (draw-func (this <ss-inspector>))
               (next-method)
               (apply set-color (slot-ref this 'color))
               (draw-all-lines this)
               )

;; Should possibly be a method
(define (draw-all-lines this)
  (map (lambda (set)
         (apply draw-line*
                (map (cut + <> (pos this))
                     set)))
       ;; TODO O(1) append
       (append (hori-lines this)
               (vert-lines this))))


;; The two *-lines functions really should be
;; merged into one. But I don't see any easy
;; and nice way of doing it right now.

(define (vert-lines this)
  (let ((height (y (get-sprite-size this))))
    (map (lambda (x)
           (list (v2 x 0) (v2 x height)))
         (map (cut * <> (x (single-size this)))
              (iota (x (amount this)))))))

(define (hori-lines this)
  (let ((width (x (get-sprite-size this))))
    (map (lambda (y)
           (list (v2 0 y) (v2 width y)))
         (map (cut * <> (y (single-size this)))
              (iota (y (amount this)))))))
