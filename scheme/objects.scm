;;; TODO this module should be renamed to objects

(define-module (objects)
  #:use-module (oop goops)
  #:use-module (vector)
  #:use-module (util)
  #:use-module (event)

  #:use-module (pos)
  #:re-export (pos)
  #:export (<game-object> name counter parent
            <geo-object>

            in-object?)
  )

;;; Primitive Data-objects
(define-class <game-object> ()
              (name #:init-keyword #:name
                    #:getter name
                    #:init-value "[NAMELESS]")
              (counter #:init-value 0
                       #:getter counter)
              ;; TODO currently parent needs to be manually set whenever
              ;; An object is created. Figure something better out.
              (parent #:init-keyword #:parent
                      #:getter parent))

(define-class <geo-object> (<game-object>)
  ;; z order of the object, used for mouse button eventts,
  ;; as well as drawing order.
  (z #:accessor z
     #:init-value 0
     #:init-keyword #:z)
  (pos #:accessor pos
       #:init-keyword #:pos
       #:init-form (make <v2> #:x 0 #:y 0)))

;;; Empty methods are to safely allow any objects to call these
;;; methods without crashing. They also provide simple expected
;;; type signatures for the methods

;;; TODO This should be together with the define-generic
(define-method (event-do (obj <game-object>)
                         (event <common-event>))
  ;; Default event, intentionally does nothing
  )

;; This should return true if v is inside the object
;; since different objects have different ways of determening size
;; every object has to define this for them selves
(define-generic in-object?)
(define-method (in-object? (o <geo-object>)
			   (v <v2>))
  #f)
