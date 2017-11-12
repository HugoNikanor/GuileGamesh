(define-module (util)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-26)
               #:export (apply-for-each cart-prod r))

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

