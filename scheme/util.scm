(define-module (util)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-26)
               #:export (apply-for-each cart-prod r slot-mod!
                                        square pi tau
                                        keyword-ref))

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


#| for future use
(define-macro (swap! a b)
  (let ((symb (gensym)))
    `(begin
       (set! ,symb ,a)
       (set!
         |#
