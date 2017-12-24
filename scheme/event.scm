(define-module
  (event)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (
            last-event
            <event>

            event-do

            ;; exported for c code?
            fix-event-args
            event-func))

(load-extension "main" "expose_event")



;; Should really be of type Maybe Event
(define last-event '())

(define-class <event> ()
              type timestamp window-id)

(define-generic fix-event-args)
(define-method (fix-event-args (_ <event>) rest))

#|
(define (event-func event)
  "Event dispatcher?"
  (apply (lambda (_ __ event-objs)
           (let ((e (eval `(make ,(car event))
                          (current-module))))
             (slot-set! e 'type (list-ref event 1))
             (slot-set! e 'timestamp (list-ref event 2))
             (slot-set! e 'window-id (list-ref event 3))
             (apply fix-event-args e (drop event 4))
             (for-each (lambda (obj)
                         (event-do obj e))
                       event-objs)))
         (current-scene)))
|#

(define-generic event-do)

#| event-func
 | Takes an incomming event, populates it with useful
 | information, such as creation time.
 |
 | Then calls all objects registered to access events
 | (from event-list) with the event.
 |#
(define (event-func event)
  (set! last-event event)
  (let ((e (eval `(make ,(car event))
                 (current-module))))
    (slot-set! e 'type (list-ref event 1))
    (slot-set! e 'timestamp (list-ref event 2))
    (slot-set! e 'window-id (list-ref event 3))
    (apply fix-event-args e (drop event 4))
    (for-each (lambda (obj)
                (event-do obj e))
              (current-eventlist))))
              ;;(get-event-list (current-scene)))))
