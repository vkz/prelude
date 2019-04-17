#lang prelude/tables

(require prelude/testing)

(define (run-tests)

  (define/checked t {('a (λ (a) (+ a 42)))
                     ('b 2)
                     ('c (λ (self key) (get: self key)))})

  (check-eq? (t.a 1) 43)

  (check-eq? t.b 2)

  (check-eq? (t:c 'b) 2)

  (check-exn exn:fail:contract? (thunk (list 1 t:d 2)) "procedure of at least 1 argument")

  (displayln "test-table-syntax ... done"))

(provide run-tests)
