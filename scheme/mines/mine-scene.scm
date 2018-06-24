(define-module (mines mine-scene)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once
  #:use-module (vector)

  #:use-module (mines mines)

  #:export (mine-scene board))

(with-new-scene
 mine-scene "Minesweeper Scene"
 (define-once board (make <mine-board>))
 (do-once
  (register-draw-object! board)))
