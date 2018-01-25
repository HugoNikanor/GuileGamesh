(define-module
  (objects sprite)
  #:use-module (oop goops)
  #:use-module (engine)

  #:export (<sprite>))

(define-class <sprite> (<geo-object>)
              sprite
              (file #:init-keyword #:file))

;; TODO
;; I want to get slot-unbound to work for `file'
;; But I can't seem to get slot-unbound to check
;; which field that wis trying to be accessed.

(define-syntax-rule (slot-ref* obj field default)
  (begin
    (catch 'goops-error
           (lambda ()
             (slot-ref obj field))
           (lambda args
             (slot-set! obj field default)))
    (slot-ref obj field)))

(define-method (initialize (this <sprite>) args)
  (next-method)
  (slot-set!
    this 'sprite
    (load-image
      (slot-ref*
        this 'file
        "assets/MissingTexture.jpg"))))

;; TODO
;; have a better way to go from <v2> to positions
;; possibly have the C functions useing slot-ref
(define-method (draw-func (this <sprite>))
               (render-sprite (slot-ref this 'sprite)
                              ((lambda (pos)
                                 (list
                                   (slot-ref pos 'x)
                                   (slot-ref pos 'y)))
                               (pos this)))
               (next-method))
  
