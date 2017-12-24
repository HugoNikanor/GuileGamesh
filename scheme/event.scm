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

#| Provides:
 | - current-eventlist
 |#
(load-extension "main" "expose_event")

#| last-event
 | Last event which was created
 | Should mainly be used for debugging
 |
 | Should really be of type Maybe Event
 |#
(define last-event '())

#| <event>
 | Event class represents SDL events
 |#
(define-class <event> ()
              type timestamp window-id)

#| fix-event-args
 | This is a fix for rebinding the SDL structs into GOOPS
 | objects. There is a foreign object interface in Guile,
 | but I can't get it to work.
 |#
(define-generic fix-event-args)
(define-method (fix-event-args (_ <event>) rest))

#| event-do
 | Method to override if you want an event to do something
 | also requires registering it in the event-list
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
