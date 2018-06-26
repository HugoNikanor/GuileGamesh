(define-module (mines helpers)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once
  #:use-module (object)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 arrays)
  #:use-module (event)
  #:use-module (vector)

  #:use-module (mines utils)
  #:use-module (mines square)
  #:use-module (mines board)

  #:export (adjacency-list falsify-zero place-numbers!
                           display-board open-squares
                           <game-end-event> reason))

;;; These are helper functions specific enough to only really
;;; be helpfull for exactly this game.

(define (adjacency-list array i j)
  (filter-cross-map (x y)
                    (lambda () (if (= x y 0) #f
                              (safe-array-ref array
                                              (+ x i)
                                              (+ y j))))
                    (list -1 0 1)
                    (list -1 0 1)))

(define (falsify-zero n)
  "0 => #f, n => n"
  (if (zero? n) #f n))

(define (place-numbers! board)
  (array-index-map!
   board
   (lambda (i j)
     (let ((sq (array-ref board i j)))
       (unless (eq? 'b (value sq))
         (set! (value sq)
               (falsify-zero (count bomb? (adjacency-list board i j)))))
       sq))))

(define* (display-board board #:optional (port #t))
  (let* ((source (tiles board))
         (target (array-copy source)))
    (array-map! target repr source)
    (apply format port "~:@{|~@{~a~}|~%~}~%"
           (array->list target))))

(define-class <game-end-event> (<common-event>)
  ;; Reason the game ended
  (reason #:init-keyword #:reason
          #:getter reason))

(define (open-squares square)
  (let ((board (parent square)))
    (set! (hidden square) #f)
    (cond ((bomb? square)
           (dispatch-event (parent board)
                           (make <game-end-event>
                             #:reason 'bomb)))
          ((empty? square)
           (map open-squares
                (filter hidden?
                        (apply adjacency-list (tiles board)
                               (v2->list (board-pos square)))))))
    (board-pos square)))
