(define-module (event key)
  #:use-module (event)
  #:use-module (oop goops)
  :use-module (ice-9 hash-table)
  #:re-export (<keyboard-event> make-keyboard-event)
  #:export (scancodes
            scan-symbol keyup? keydown?
            )
  )

;;; TODO stop exporting scancodes, currently used for scene changed

(define scancodes
  '((n . 15)))

(define scancodes-hash
  (alist->hash-table
   '((79 . right)
     (80 . left)
     (81 . down)
     (82 . up)
     )))

(define (scan-symbol ev)
  (hash-ref scancodes-hash
            (slot-ref ev 'scancode)
            #f))

(define (keyup? ev)
  (eq? 'SDL_KEYUP (slot-ref ev 'type)))

(define (keydown? ev)
  (eq? 'SDL_KEYDOWN (slot-ref ev 'type)))
