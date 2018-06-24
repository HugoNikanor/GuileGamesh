(define-module (mines helpers)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once
  #:use-module (object)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 arrays)

  #:use-module (mines utils)
  #:use-module (mines square)
  #:use-module (mines board)

  #:export (adjacency-list falsify-zero place-numbers!
                           display-board open-square))

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


;; Return of #f indicates that opening should stop,
;; Return of #t indicates that opening should continue
;; to adjacent tiles
(define (open-square square)
  (when (hidden? square)
    (set! (hidden square) #f)
    (let ((v (value square)))
      (cond ((eqv? v 'b) (display "BOOM!\n") #f)
            ((sq:number? v) #f)
            ((eqv? v #f) #t)))))
