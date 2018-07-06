(define-module (mines scene)
  #:use-module (common)
  #:use-module (oop goops)
  #:use-module (scene) ;; with-new-scene

  #:use-module (event mouse-btn)

  #:use-module (mines board)
  #:use-module (mines helpers) ; <game-end-event>
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

(define-method (event-do (this <mine-scene>)
                         (event <scene-changed-out-event>))
  (set-window-size! 512 512))

(define-method (event-do (this <mine-scene>)
                         (event <game-end-event>))
  (format #t "The game ended due to ~a~%" (reason event)))


(with-new-scene
 mine-scene "Minesweeper Scene"
 (define-once board (make <mine-board>
                      #:size (v2 5 5)
                      #:parent mine-scene))

 ;; This is here for debugging
 #;
 (array-for-each (lambda (sq) (set! ((@ (mines square) hidden) sq) #f))
                 (tiles board))

 (do-once
  (change-class mine-scene <mine-scene>)

  (register-draw-object! board)

  (array-for-each
   (cut add-event-listener! <mouse-button-event> <>)
   (tiles board))

  (add-event-listener! <game-end-event> mine-scene)

  (add-event-listener! <scene-changed-in-event> mine-scene)
  (add-event-listener! <scene-changed-out-event> mine-scene)))


