(define-module (tile)
               #:use-module (oop goops)
               #:use-module (srfi srfi-26)
               #:export (<tile-set>
                          get-tile-sheet
                          get-tile-size
                          board-size))

(define-class <tile-set> (<game-object>)
              tile-sheet ; SDL_texture
              tile-size ; 16
              board-size ; v2 16 16 (in tiles)
              )

(define tileset (make <tile-set>))
(slot-set! tileset 'tile-sheet
           (load-image "assets/PathAndObjects_0.png"))
(slot-set! tileset 'tile-size 16)
(slot-set! tileset 'board-size '(16 16))

(define (apply-for-each func lists)
  (unless (null? lists)
    (apply func (car lists))
    (apply-for-each func (cdr lists))))


(define (cart-prod l1 l2)
  (map (lambda (item)
         (map (cut list item <>)
              l2))
       l1))

(define-method
  (draw-func (tileset <tile-set>))
  (apply-for-each
    (lambda (x y)
      (render-texture tile-sheet
                      tile-size
                      (0 0) ; sprite position in sheet 
                      (x y))
      )
    (cart-prod (iota (x board-size))
               (iota (y board-size)))))

