(define-module (keylistener)
  #:use-module (oop goops)
  #:use-module (engine)
  #:use-module (event) ;; is this needed?
  #:use-module (event key)
  #:use-module (object)
  #:use-module (scene)
  #:export (<key-listener>))

(define-class <key-listener> (<game-object>)
  (obj #:init-keyword #:obj
       ;; should error if not set
       )
  (map #:init-keyword #:map
       #:init-value (lambda (e) e)
       ))

(define-method (initialize (lis <key-listener>) initargs)
  (next-method)
  (slot-set! lis 'name
             "[KEY LISTENER ~a]")
  ;;(format #f "[KEY LISTENER ~a]"
  ;; object is not yet initialized, name doesn't exist
  ;;(object-name (slot-ref lis 'obj))))
  (add-event-listener! <keyboard-event> lis))

(define-method (event-do (listener <key-listener>)
                         (event <keyboard-event>))
  ((slot-ref listener 'map)
   (slot-ref listener 'obj)
   event))

