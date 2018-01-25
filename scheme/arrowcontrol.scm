(define-module (arrowcontrol)
               #:use-module (oop goops)
               #:use-module (srfi srfi-26)
               #:use-module (util) ;; slot-mod!
               #:use-module (engine) ;; pos
               #:use-module (event)
               #:use-module (event key)
               #:use-module (keylistener)
               #:export (<arrow-control>))

;; Object which derives arrow-control should also
;; derive <geo-object>
(define-class <arrow-control> ()
             key-listener)

;; TODO the slot-set! here should probably be set in
;;      the class definition.
;; This method should however probably register the
;; objects to the correct {event,tick}-list.
(define-method (initialize (obj <arrow-control>) initargs)
               (slot-set! obj 'key-listener
                          (make <key-listener>
                                #:obj obj
                                #:map key-map))
               (next-method))

(define (key-map obj event)
  (case (slot-ref event 'scancode)
    ((82)  (slot-mod! (pos obj) 'y (cut - <> 5) ));1- )) ;;
    ((81)  (slot-mod! (pos obj) 'y (cut + <> 5) ));1+ ))
    ((79)  (slot-mod! (pos obj) 'x (cut + <> 5) ));1+ )) ;;
    ((80)  (slot-mod! (pos obj) 'x (cut - <> 5) ));1- ))
    (else #f)))

(define arrow-up 82)
(define arrow-down 79)
(define arrow-left 81)
(define arrow-right 80)
