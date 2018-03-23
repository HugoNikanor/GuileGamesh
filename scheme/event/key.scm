(define-module (event key)
  #:use-module (event)
  #:use-module (oop goops)
  #:re-export (<keyboard-event> make-keyboard-event)
  #:export (scancodes)
  )

;;; (assv-ref scancodes 'a)
;(define scancodes
;  '((a . 4)  (b . 5)  (c . 6)  (d . 7)  (e . 8)
;    (f . 9)  (g . 10) (h . 11) (i . 12) (j . 13)
;    (k . 14) (l . 15) (m . 16) (n . 17) (o . 18)
;    (p . 19) (q . 20) (r . 21) (s . 22) (t . 23)
;    (u . 24) (v . 25) (w . 26) (x . 27) (y . 28)
;    (z . 29)))

(define scancodes
  '((n . 15)))

;; (define-class <key-event> (<event>) state repeat scancode sym mod)
;; (define-method (fix-event-args (ev <key-event>) state repeat keysym)
;;                (apply (lambda (scancode sym mod)
;;                         (slot-set! ev 'state state)
;;                         (slot-set! ev 'repeat repeat)
;;                         (slot-set! ev 'scancode (list-ref keysym 0))
;;                         (slot-set! ev 'sym (list-ref keysym 1))
;;                         (slot-set! ev 'mod (list-ref keysym 2)))
;;                       keysym))
