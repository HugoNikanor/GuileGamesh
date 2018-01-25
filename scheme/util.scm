(define-module (util)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-26)
               #:export (apply-for-each cart-prod r slot-mod!
                                        square pi tau
                                        keyword-ref
                                        do-once))

(define (r)
  (system "reset"))

(define (apply-for-each func lists)
  (unless (null? lists)
    (apply func (car lists))
    (apply-for-each func (cdr lists))))


(define (cart-prod l1 l2)
  (apply append
         (map (lambda (item)
                (map (cut list item <>)
                     l2))
              l1)))

(define-macro (slot-mod! obj slot func)
  `(slot-set! ,obj ,slot (,func (slot-ref ,obj ,slot))))

;;; (define (get-largest-by func . items)
;;;   (map (lambda (item)
;;;          (cons item (func item)))

(define (square x)
  (* x x))

(define pi 3.141592653589793)
(define tau (* pi 2))

(define (keyword-ref keyword list)
  "Get value after keyword in list, default to #f:
  (keyword-ref #:b '(#:a 1 #:b 2 #:c 3)) => 2
  (keyword-ref #:b '()) => #f"
  (and=> (member keyword list)
         cadr))

;; *loaded* isn't gensymed because it needs to be
;; the same symbol every time this is run.
(define-macro (do-once . body)
  "Like define-once, but for an entire code block
   Note that definitions can't be present within
   this body"
  `(begin
     (define-once *loaded* #f)
     (unless *loaded*
       (set! *loaded* #t)
       (display "The module is now loading\n")
       ,@body)))


#| for future use
(define-macro (swap! a b)
  (let ((symb (gensym)))
    `(begin
       (set! ,symb ,a)
       (set!
         |#
