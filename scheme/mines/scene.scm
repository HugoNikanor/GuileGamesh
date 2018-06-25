(define-module (mines scene)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once

  #:use-module (vector)

  #:use-module (mines board)
  #:use-module (mines square) ; size

  #:export (mine-scene board))

;; This is so that this scene can have special
;; handling of events.
;; TODO figure out something better for this.
(define-class <mine-scene> (<scene>))

(define-method (event-do (this <mine-scene>)
                         (event <scene-changed-in-event>))
  (let* ((cell-size (size (array-ref (tiles board) 0 0)))
         (board-size (* cell-size (tilecount board))))
    (apply set-window-size! (v2->list board-size))))


(with-new-scene
 mine-scene "Minesweeper Scene"
 (define-once board (make <mine-board>))

 ;; This is here for debugging
 (array-for-each (lambda (sq) (set! ((@ (mines square) hidden) sq) #f))
                 (tiles board))

 (do-once
  (change-class mine-scene <mine-scene>)
  (register-draw-object! board)
  (add-event-listener! <scene-changed-in-event> mine-scene)))


