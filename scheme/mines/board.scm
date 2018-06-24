(define-module (mines board)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once
  #:use-module (object)

  #:use-module (vector)
  #:use-module (oop goops describe)
  #:use-module (ice-9 format)
  #:use-module (ice-9 arrays)

  #:use-module (mines square)
  #:use-module (mines helpers)

  #:export (<mine-board> tiles tilecount))

(define-class <mine-board> (<geo-object>)
  (tilecount #:init-form (v2 10 10)
             #:accessor tilecount
             #:init-keyword #:size)
  (tiles #:getter tiles))

(define-method (draw-func (this <mine-board>))
  (next-method)
  ;; TODO there should be an "with transform/offset",
  ;; which would allow all the child tiles to be moved
  ;; with the board 
  (array-for-each draw-func (tiles this)))

(define-method (initialize (this <mine-board>) args)
  (next-method)
  (let ((arr (apply make-array 10 (v2->list (tilecount this)))))
    (array-map! arr make-square arr)
    (place-numbers! arr)
    (let ((size (size (array-ref arr 0 0))))
      (array-index-map! (array-copy arr) (lambda (i j)
                              (set! (pos (array-ref arr i j))
                                    (v2 (* size i)
                                        (* size j))))))
    (slot-set! this 'tiles arr)))


(define-method (describe (this <mine-board>))
  (next-method)
  (newline)
  (display-board this))
