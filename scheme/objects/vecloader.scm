(define-module (objects vecloader)
               #:use-module (ice-9 match)
               #:use-module (oop goops)
               #:use-module (vector)
               #:use-module (engine)
               #:export (<line> <vec-graphics> parse)
               )

;; (define file (open-input-file "assets/obj.sxml"))

(define-class <line> ()
              (start #:init-keyword #:start)
              (stop  #:init-keyword #:stop))

(define-class <vec-graphics> (<geo-object>)
              (anchor #:init-keyword #:anchor
                      #:init-form (make <v2> #:x 0 #:y 0))
              (lines #:init-keyword #:lines
                     #:init-value '()))

(define-method (draw-func (obj <vec-graphics>))
  (let ((anchor (slot-ref obj 'anchor))
        (pos (slot-ref obj 'pos)))
    (for-each
      (lambda (line)
        (let ((shift (lambda (v) (+ v (+ pos anchor)))))
          (let ((x0 (x (shift (slot-ref line 'start))))
                (y0 (y (shift (slot-ref line 'start))))
                (x1 (x (shift (slot-ref line 'stop))))
                (y1 (y (shift (slot-ref line 'stop)))))
            (draw-line x0 y0 x1 y1))))
      (slot-ref obj 'lines)))
  (next-method))

(define (parse file)
  (let* ((struct (read file))
         (anchor (cadr (list-ref struct 1)))
         (lines (cddr struct)))
    (let ((lineobjs
            (map
              (match-lambda (('line `(@ (from ,from)
                                        (to ,to)))
                             (make <line>
                                   #:start (make <v2> from)
                                   #:stop  (make <v2> to))))
              lines)))
      (make <vec-graphics>
            #:anchor (make <v2> anchor)
            #:lines lineobjs))))
