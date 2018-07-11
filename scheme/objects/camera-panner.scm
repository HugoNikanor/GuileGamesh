(define-module (objects camera-panner)
  :use-module (common)
  :use-module (event)
  :use-module (event key)
  :use-module (scene)
  :export (make-camera-panner))

(define-class <camera-panner> (<game-object>)
  (camera :init-keyword :camera
          :getter camera
          :init-form (current-camera)
          )
  (keys :init-keyword :keys
        :init-value '(up down left right))
  (directions :init-form (list (v2 0 -1) ; up
                               (v2 0 1)  ; down
                               (v2 -1 0) ; left
                               (v2 1 0))); right

  (pan-speed :init-keyword :speed
             :accessor pan-speed
             :init-value 1)
  (keybinds :getter keybinds
            :allocation :virtual
            :slot-ref (lambda (this)
                        (map cons
                             (slot-ref this 'keys)
                             (slot-ref this 'directions)))
            :slot-set! (lambda (this new) #f)))

(define* (make-camera-panner :optional camera)
  (if camera
      (make <camera-panner> :camera camera)
      (make <camera-panner>)))

(define-method (event-do (this <camera-panner>)
                         (ev <keyboard-event>))
  (when (keyup? ev)
    (let ((c (camera this)))
      (set! (pos c)
            (+ (pos c)
               (* (pan-speed this)
                  (or (assoc-ref (keybinds this)
                                 (scan-symbol ev))
                      (v2))))))))
