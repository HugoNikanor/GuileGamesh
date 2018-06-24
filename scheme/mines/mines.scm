(define-module (mines mines)
  #:use-module (oop goops) ;; make
  #:use-module (oop goops describe)
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once
  #:use-module (event)
  #:use-module (event mouse-btn)
  #:use-module (object)
  #:use-module (objects multi-sprite)

  #:use-module (vector)
  #:use-module (draw-help)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 format)
  #:use-module (ice-9 arrays)

  #:export (<mine-board> tilecount tiles
                         hidden value flag))

(define-class <square> (<multi-sprite>)

  ;; LOGICAL FIELDS 

  ;; #f, #t
  (hidden #:init-value #t
          #:accessor hidden)
  ;; TODO change #f to just be 0
  ;; #f, 'b, 0-8
  (value #:init-value #f
         #:accessor value)
  ;; #f, 'f, '?
  (flag #:init-value #f
        #:accessor flag)



  (current-sprite #:allocation #:virtual
                  #:accessor current-sprite
                  #:slot-ref (lambda (this)
                               (if (hidden? this)
                                   (case (flag this)
                                     ((f) 10)
                                     (else 11))
                                   (case (value this)
                                     ((#f) 0)
                                     ((b) 9)
                                     (else (value this)))))
                  #:slot-set! (lambda (this new)
                                '(set-value "Can't set value")))
  )

#;
(define-method (access-sprite (this <square>)
                              current-sprite)
  (list-ref (sprite-list this)
            current-sprite))

;;; TODO the sprite and name list is currently not shared between objects,
;;; I think that there is some kind of static solution to this.

(define* (make-square #:optional (bomb-probability 10))
  (let ((sq (make <square> #:files
                  (map (cut string-append "assets/mine-tiles/" <> ".png")
                       '("0" "1" "2" "3" "4" "5" "6" "7" "8"
                         "mine" "flag" "unexplored"))
                  )))
    (when (< 7 (random bomb-probability))
      (set! (value sq) 'b))
    sq))

(define-method (in-object? (this <square>)
                           (point <v2>))
  (let* ((p (pos this))
         (u (+ p (size this))))
    (< p point u)))

(define hidden? hidden)
(define (bomb? square)
  (eq? 'b (value square)))
(define (empty? square)
  (not (value square)))

(define-method (sq:number? (square <square>))
  (if (number? (value square))
      (value square)
      #f))

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

(define-method (event-do (this <square>)
                         (event <mouse-button-event>))
  (when (in-object? this (+ (pos this)
                            (pos event)))
    (display "Pressed a square\n")
    (cond
     ((lclick? event))
     ((rclick? event)))))

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

;;; Some of these should be moved to the util package

(define (safe-array-ref array . idx)
  "Like array-ref, but returns #f if not in bounds"
  (if (apply array-in-bounds? array idx)
      (apply array-ref array idx)
      #f))

;; Maps over cross products of the arguments
;; TODO This should be changed to use the argument list of the lambda
(define-syntax cross-map
  (syntax-rules ()
    ((_ (symb) thunk lst)
     (map (lambda (symb) (thunk))
          lst))
    ((_ (symb symbs ...) thunk lst lsts ...)
     (apply append
            (map (lambda (symb)
                   (cross-map (symbs ...)
                              thunk lsts ...))
                 lst)))))

(define (filter-map proc . lst)
  (filter identity (apply map proc lst)))

(define-syntax-rule (filter-cross-map (symb ...) thunk lst ...)
  (filter identity (cross-map (symb ...)
                              thunk lst ...)))

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

(define-generic repr)
(define-method (repr (sq <square>))
  (if (hidden? sq)
      (case (flag sq)
        ((f) 'F)
        ((?) '?)
        (else #\space))
      (cond ((bomb? sq) 'B)
            ((sq:number? sq) => (lambda (n) (if (zero? n) #\~ n)))
            (else #\space))))

(define* (display-board board #:optional (port #t))
  (let* ((source (tiles board))
         (target (array-copy source)))
    (array-map! target repr source)
    (apply format port "~:@{|~@{~a~}|~%~}~%"
           (array->list target))))

;; (format #t "~:@{|~@{~a~}|~%~}~%" '(1 2) '(#\space b))
;; |12|
;; | b|


(define-method (describe (this <mine-board>))
  (next-method)
  (newline)
  (display-board this))

(define (do)
  (let ((b (make <mine-board>)))
    (array-for-each (lambda (sq) (set! (hidden sq) #f))
                    (tiles b))
    (describe b)
    b))
#; 
(array-for-each (lambda (sq) (set! (hidden sq) #f))
                    (tiles board))
