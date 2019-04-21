#lang prelude/tables

(require prelude/testing
         syntax/macro-testing)

(provide run-tests
         run-define/table-tests)

(define (run-tests)

  (define/checked t {('a (λ (a) (+ a 42)))
                     ('b 2)
                     ('c (λ (self key) (get: self key)))})

  (check-eq? (t.a 1) 43)

  (check-eq? t.b 2)

  (check-eq? (t:c 'b) 2)

  (check-exn exn:fail:contract? (thunk (list 1 t:d 2)) "procedure of at least 1 argument"))


(module+ test
  (run-tests))


(define (run-define/table-tests)
  (define/checked tbl {})
  (void (checked (define/table tbl.key 42)))
  (void (checked (define/table (tbl.f arg) (+ 1 arg))))
  (void (checked (define/table (tbl:meth key) self.key)))
  (check-eq? (tbl.f 42) 43)
  (check-eq? (tbl:meth 'key) 42)

  ;; NOTE unbound identifier is a compile time error that we can't just merily
  ;; catch at runtime
  (check-exn exn? (thunk (convert-compile-time-error
                          (define/table undefined-t.foo 42)))))


(module+ test
  (run-define/table-tests))

(module+ test
  (test-case "{ } constructor must catch bad syntax"
    (check-exn exn:fail:syntax? (thunk (convert-compile-time-error {42 ('key 'val)})) #rx"expected key-value")
    (check-exn exn:fail:syntax? (thunk (convert-compile-time-error {('key 'val) 'foo})) #rx"expected key-value")))
