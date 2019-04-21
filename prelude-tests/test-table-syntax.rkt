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
  ;; method of arity 0, takes only self
  (void (checked (define/table (tbl:meth0) self.key)))
  ;; method of arity > 0, takes self and more args
  (void (checked (define/table (tbl:meth1 key) self.key)))
  ;; method of arity 0 with keyword args
  (void (checked (define/table (tbl:kwmeth0 #:key [key 'key]) (get: self key))))
  ;; method of arity > 0 with keyword args
  (void (checked (define/table (tbl:kwmeth1 inc #:key [key 'key]) (+ inc (or (get: self key) 0)))))

  ;; key lookup
  (check-eq? tbl.key 42)
  ;; function call
  (check-eq? (tbl.f 42) 43)
  ;; arity 1 method call
  (check-eq? (tbl:meth1 'key) 42)
  (check-eq? (let ((method tbl:meth1)) (method 'key)) 42)
  ;; arity 0 method call
  (check-eq? (tbl:meth0) 42)
  (check-eq? (let ((method tbl:meth0)) (method)) 42)
  ;; arity 0 with keyword args method call
  (check-eq? (tbl:kwmeth0) 42)
  (check-eq? (tbl:kwmeth0 #:key 'key) 42)
  (check-eq? (tbl:kwmeth0 #:key 'nokey) undefined)
  (check-eq? (let ((method tbl:kwmeth0)) (method)) 42)
  (check-eq? (let ((method tbl:kwmeth0)) (method #:key 'key)) 42)
  ;; arity 1 with keyword args method call
  (check-eq? (tbl:kwmeth1 1) 43)
  (check-eq? (tbl:kwmeth1 1 #:key 'key) 43)
  (check-eq? (tbl:kwmeth1 1 #:key 'nokey) 1)
  (check-eq? (let ((method tbl:kwmeth1)) (method 1)) 43)
  (check-eq? (let ((method tbl:kwmeth1)) (method 1 #:key 'key)) 43)

  ;; arity mismatch
  (check-exn exn:fail:contract:arity? (thunk (tbl:meth1)) "arity mismatch")
  (check-exn exn:fail:contract:arity? (thunk (tbl:kwmeth1)) "arity mismatch")

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
