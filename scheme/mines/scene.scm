(define-module (mines scene)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once

  #:use-module (mines board)

  #:export (mine-scene board))

(with-new-scene
 mine-scene "Minesweeper Scene"
 (define-once board (make <mine-board>))

 ;; This is here for debugging
 (array-for-each (lambda (sq) (set! ((@ (mines square) hidden) sq) #f))
                 (tiles board))

 (do-once
  (register-draw-object! board)))


