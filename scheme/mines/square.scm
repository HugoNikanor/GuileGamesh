(define-module (mines square)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once
  #:use-module (objects)

  #:use-module (event)
  #:use-module (event mouse-btn)
  #:use-module (objects multi-sprite)
  #:use-module (vector)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)

  #:use-module (mines helpers)

  #:export (<square> board-pos
            make-square
            hidden value flag
            repr

            ;; These maybe shouldn't be exported:
            hidden? bomb? empty? flag? sq:number?)
  #:re-export (size ; from multi-sprite
               current-sprite
               ))

(define-class <square> (<multi-sprite>)
  ;; #f, #t
  (hidden #:init-value #t
          #:accessor hidden)
  ;; TODO change #f to just be 0
  ;; #f, 'b, 0-8
  (value #:init-value #f
         #:accessor value)
  ;; #f, #t
  (flag #:init-value #f
        #:accessor flag)

  (current-sprite #:allocation #:virtual
                  #:accessor current-sprite
                  #:slot-ref (lambda (this)
                               (if (hidden? this)
                                   (if (flag this)
                                       10 11)
                                   (case (value this)
                                     ((#f) 0)
                                     ((b) 9)
                                     (else (value this)))))
                  #:slot-set! (lambda (this new)
                                '(set-value "Can't set value")))

  (board-pos #:allocation #:virtual
             #:accessor board-pos
             #:slot-ref (lambda (this)
                          (m/ (pos this)
                              (v2 (size this)
                                  (size this))))
             #:slot-set! (lambda (this new)
                           (slot-set! this 'pos (* (size this) new)))))

;;; TODO the sprite and name list is currently not shared between objects,
;;; I think that there is some kind of static solution to this.

;; parent is the board the square resides in.
(define* ((make-square parent) #:optional (bomb-probability 10))
  "Creates a new square, and maybe add a bomb to it."
  (let ((sq (make <square> #:parent parent)))
    (when (< 7 (random bomb-probability))
      (set! (value sq) 'b))
    sq))

(define-method (initialize (this <square>) args)
  "Set file-list to predetermined, sending #:files
argument is an error."
  (slot-set! this 'file-list
             (map (cut string-append "assets/mine-tiles/" <> ".png")
                  '("0" "1" "2" "3" "4" "5" "6" "7" "8"
                    "mine" "flag" "unexplored")))
  (next-method))

(define-method (in-object? (this <square>)
                           (point <v2>))
  (let* ((p (pos this))
         (u (+ p (v2 (size this)
                     (size this)))))
    ;; TODO this is really uggly
    (< (- p (v2 1 1)) point u)))

(define-method (event-do (this <square>)
                         (event <mouse-button-event>))
  (when (up? event)
    #; (set! (hidden this) (not (hidden this)))
    (cond
     ((lclick? event)
      (open-squares this))
     ((rclick? event)
      (set! (flag this) (not (flag this)))))))

(define-method (repr (sq <square>))
  (if (hidden? sq)
      (if (flag? sq)
          'F #\space)
      (cond ((bomb? sq) 'B)
            ((sq:number? sq) => (lambda (n) (if (zero? n) #\~ n)))
            (else #\space))))

(define hidden? hidden)

(define (bomb? square)
  (eq? 'b (value square)))

(define (empty? square)
  (not (value square)))

(define flag? flag)

(define (sq:number? square)
  (if (number? (value square))
      (value square)
      #f))
