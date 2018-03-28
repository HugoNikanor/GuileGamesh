(use-modules (system repl server))

#| Connect with:
- Shell
 $ nc -U *path*
- Emacs
 (geiser-connect-local 'guile *path*)
|#

(define-once *path* "/tmp/guile-gamesh-repl")

(system* "rm" *path*)

(spawn-server (make-unix-domain-server-socket #:path *path*))
