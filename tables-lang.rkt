#lang racket

(require prelude/prelude-lang "tables.rkt")

(provide (except-out (all-from-out prelude/prelude-lang) #%app #%top)
         (except-out (all-from-out "tables.rkt") app top)
         (rename-out [app #%app]
                     [top #%top]))
