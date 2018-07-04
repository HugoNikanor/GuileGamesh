(define-module (mines utils)
  #:use-module (oop goops) ;; make
  #:use-module (engine) ;; register-*-object!
  #:use-module (scene) ;; with-new-scene
  #:use-module (util) ;; do-once
  #:use-module (objects)

  #:use-module (srfi srfi-1)

  #:export (safe-array-ref
            cross-map filter-map
            filter-cross-map))

;;; These should be merged into the main util package,
;;; Once they are tested.

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
