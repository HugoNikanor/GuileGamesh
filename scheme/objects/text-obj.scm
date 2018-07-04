(define-module (objects text-obj)
  #:use-module (oop goops)
  #:use-module (objects)
  #:export (<text-obj>))

;;; Object for displaying simple text objects on
;;; the screen. Currently broken
(define-class <text-obj> (<geo-object>)
              (text #:init-value " "
                    #:init-keyword #:str)
              (update-text #:init-value #f
                           #:init-keyword #:update))
