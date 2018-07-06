#|
 | This file is for setting up the repl environment, 
 | as well as starting the system.
 | 
 | ready! ought to be called from the C main code,
 | but I haven't got that to work yet.
 |#

(add-to-load-path "scheme")

(use-modules (engine))

(set! *random-state* (random-state-from-platform))

(ready!)

(use-modules (gamesh))

(set-current-module (resolve-module '(gamesh)))

