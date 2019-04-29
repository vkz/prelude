#lang prelude/tables

(require prelude/testing
         syntax/macro-testing)

(provide run-basic-table-tests
         run-define/table-tests
         run-simple-inheritance-tests
         run-multiple-inheritance-tests
         run-tags-tests)


(define (run-basic-table-tests)

  (define/checked t {('a (λ (a) (+ a 42)))
                     ('b 2)
                     ('c (λ (self key) (get: self key)))})

  (check-eq? (t.a 1) 43)
  (check-eq? t.b 2)
  (check-eq? (t:c 'b) 2)
  (check-exn exn:fail:contract? (thunk (list 1 t:d 2)) "procedure of at least 1 argument"))


(module+ test
  (run-basic-table-tests))


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
    (check-exn exn:fail:syntax? (thunk (convert-compile-time-error {42 ('key 'val)})) "expected (key value) pair")
    (check-exn exn:fail:syntax? (thunk (convert-compile-time-error {('key 'val) 'foo})) "expected (key value) pair")))


(define (run-simple-inheritance-tests)

  (define/checked Account {('balance 0)})

  (void (checked (define/table (Account:withdraw v)
                   (set: self 'balance (- self.balance v))
                   self)))

  (void (checked (define/table (Account:deposit v)
                   (set: self 'balance (+ self.balance v))
                   self)))

  (void (checked (define/table (Account:new [o {}])
                   (set: self '__index self)
                   (set-meta-table! o self)
                   o)))


  (test-case "invoke simple methods"

    ;; initial balance
    (check-eq? 0 Account.balance)

    ;; deposit
    (void (checked (Account:deposit 100)))
    (check-eq? Account.balance 100)

    ;; withdraw
    (void (checked (Account:withdraw 100)))
    (check-eq? Account.balance 0))


  ;; inherit from another table ("class")
  (define/checked LimitedAccount (Account:new))
  (check-true (table? LimitedAccount))


  ;; instantiate from a "class"
  (define/checked s (LimitedAccount:new {('limit 1000)}))


  (test-case "simple inheritance"

    (check-eq? s.limit 1000)
    (check-eq? s.balance 0)

    ;; deposit
    (void (s:deposit 100))
    (check-eq? s.balance 100)

    ;; withdraw
    (void (s:withdraw 100))
    (check-eq? s.balance 0))


  (test-case "add new method to prototype"

    ;; add method to prototype
    (void (checked (define/table (LimitedAccount:get-limit)
                     (or self.limit 0))))

    ;; should be visible in the instance
    (check-eq? (s:get-limit) 1000))


  (test-case "override inherited method"

    ;; method override
    (void (checked (define/table (LimitedAccount:withdraw v)
                     (if (> (- v self.balance) (self:get-limit))
                         (error "insufficient funds")
                         (set: self 'balance (- self.balance v))))))

    (check-exn exn? (thunk (s:withdraw 1100)) "insufficient funds")
    (check-eq? (begin (s:withdraw 500) s.balance) -500))


  (test-case "extend inheritance chain"

    ;; inherit
    (define/checked OverdraftAccount (LimitedAccount:new {('fee 5)}))
    (check-true (table? OverdraftAccount))

    ;; instantiate
    (define/checked d (OverdraftAccount:new))
    (check-eq? d.fee 5)

    ;; override method but delegate to older
    (void (checked (define/table (OverdraftAccount:withdraw v)
                     ;; TODO we need a cleaner way to delegate to prototypes
                     (let ((delegate (get-meta-table
                                      (get-meta-table self))))
                       (delegate.withdraw self (+ self.fee v))))))

    (check-eq? d.withdraw OverdraftAccount.withdraw)
    (check-eq? d.withdraw (get: (get-meta-table d) 'withdraw))

    (check-exn exn? (thunk (d:withdraw 10)) "insufficient funds")
    (check-eq? d.balance 0)

    (void (set: d 'limit 100))
    (void (checked (d:withdraw 10)))
    (check-eq? d.balance -15)))


(module+ test
  (run-simple-inheritance-tests))


(define (run-multiple-inheritance-tests)

  ;; NOTE Example straight from Lua book. It works but is hardly the way to do it
  ;; in practice. At the very minimum there is a question of identity, that is any
  ;; instance should better be able to identify as any of its parents, in this
  ;; case account isa Named and isa Account. It also bakes in the way keys are
  ;; looked up down the inheritance chain. account:new would simply create another
  ;; instance of the same class NamedAccount - might not be the best use for it.
  ;; Probably other issues, too. Decent test case, though.

  (define (create-class . parents)
    (define mt {})
    (define/table (mt:__index key)
      (for/first ((parent (in-list parents))
                  #:when (not (undefined? (get: parent key))))
        (get: parent key)))

    (define class {})
    (set: class '__index class)
    (set-meta-table! class mt)
    (define/table (class:new [t {}])
      (set-meta-table! t class)
      t)
    class)

  (define/checked Account {('balance 0)})
  (void (checked (define/table (Account:get-balance) self.balance)))

  (define/checked Named {})
  (void (checked (define/table (Named:get-name) self.name)))

  (define/checked NamedAccount (create-class Account Named))
  (define/checked account (NamedAccount:new {('name "Mike")}))
  (check-equal? (account:get-name) "Mike")
  (check-eq? (account:get-balance) 0))


(module+ test
  (run-multiple-inheritance-tests))


(define (run-tags-tests)
  ;; :tags
  (check-eq? ':tag :tag)
  (check-eq? (get: {(:key 42)} :key) 42)

  (check-true (tag? :tag))
  (check-true (tag? ':tag))
  (check-false (tag? 'tag))

  (check-equal? ":tag" (tag->string :tag))
  ;; checks contract
  (check-exn exn:fail:contract? (thunk (tag->string 'tag))))


(module+ test
  (run-tags-tests))
