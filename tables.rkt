#lang prelude


(require racket/struct
         racket/generic
         (for-syntax syntax/parse
                     racket/match))


;;* Provides -------------------------------------------------------- *;;


(provide table table? set-meta-table! rawset! app top
         define/table)


(module+ test
  (require rackunit)
  (require prelude/testing))


;; TODO we must hide all lua struct fields, getters and setters - anything that
;; may hint to the user of prelude that tables aren't just values but structs.
;; IIUC struct and provide have necessary facilities, just need to use em.


;;* Lua tables ------------------------------------------------------ *;;


(struct lua (table meta-table) #:mutable

  #:property prop:procedure dict-ref
  #:property prop:object-name (λ (self) (dict-ref self '__id))

  #:methods gen:dict

  ((define (dict-ref dict key [default (λ () undefined)])
     (if (hash-has-key? (lua-table dict) key)
         (hash-ref (lua-table dict) key)
         (if (lua-meta-table dict)
             (let ((index (dict-ref (lua-meta-table dict) '__index)))
               (cond ((lua? index)
                      (dict-ref index key))

                     ((procedure? index)
                      ;; TODO signal arity error if incorrect
                      (index dict key))

                     (else undefined)))
             ;; no such key => undefined
             undefined)))

   (define/generic super-dict-set!           dict-set!)
   (define/generic super-dict-has-key?       dict-has-key?)
   (define/generic super-dict-remove!        dict-remove!)
   (define/generic super-dict-iterate-first  dict-iterate-first)
   (define/generic super-dict-iterate-next   dict-iterate-next)
   (define/generic super-dict-iterate-key    dict-iterate-key)
   (define/generic super-dict-iterate-value  dict-iterate-value)

   ;; TODO I don't think I like dict interface very much. Wouldn't I rather have
   ;; set! and remove! return something other than void? Returning table itself
   ;; would certainly help chaining e.g. setting multiple keys in (-> ...)

   (define (dict-has-key? dict key)
     (hash-has-key? (lua-table dict) key))

   ;; TODO dict-has-key? reports unimplemented even though we supply hash-ref. So
   ;; for now we use hash-has-key? instead
   (define (dict-set! dict key v)
     (if (undefined? v)
         ;; inserting undefined for a value removes the key entry
         (super-dict-remove! (lua-table dict) key)
         ;; any other value
         (if (hash-has-key? (lua-table dict) key)
             ;; when key present simply set it
             (super-dict-set! (lua-table dict) key v)
             ;; when key absent try __newindex metamethod first else set the key
             (if (lua-meta-table dict)
                 (let ((newindex (dict-ref (lua-meta-table dict) '__newindex)))
                   (cond ((lua? newindex)
                          ;; TODO __newindex is a table then set the key there, but
                          ;; is that reasonable?
                          (dict-set! newindex key v))

                         ((procedure? newindex)
                          ;; TODO signal arity error if incorrect
                          (void (newindex dict key v)))

                         (else
                          (void (set: (lua-table dict) key v)))))
                 ;; no meta-table so we simply set the key to value
                 (super-dict-set! (lua-table dict) key v)))))

   (define (dict-remove! dict key)
     (super-dict-remove! (lua-table dict) key))

   (define (dict-iterate-first dict)
     (super-dict-iterate-first (lua-table dict)))

   (define (dict-iterate-next dict pos)
     (super-dict-iterate-next (lua-table dict) pos))

   (define (dict-iterate-key dict pos)
     (super-dict-iterate-key (lua-table dict) pos))

   (define (dict-iterate-value dict pos)
     (super-dict-iterate-value (lua-table dict) pos)))

  #:methods gen:custom-write

  ;; TODO not quite right cause (k v) pairs are printed in quoted list form, so
  ;; reading constructor back errors instead of creating a table. Need a truly
  ;; custom writer proc here.
  ((define write-proc
     (make-constructor-style-printer
      (λ (self) 'table)
      (λ (self) (for/list (((k v) (in-dict (lua-table self))))
                  (list k v)))))))


(define table? lua?)


(define-syntax-rule (table arg ...)
  (lua (ht arg ...) undefined))


(define (set-meta-table! t mt)
  (set-lua-meta-table! t mt)
  t)


(define (rawset! t k v)
  (hash-set! (lua-table t) k v))


(module+ test

  (define/checked t (table ('a 1) ('b 2)))

  (check eq? 1 (t 'a))
  (check eq? 2 (t 'b))
  (check eq? 1 (get: t 'a))
  (check eq? 2 (get: t 'b))
  (check-pred undefined? (t 'c))

  (dict-set! t 'c 42)
  (check eq? 42 (t 'c))

  (dict-remove! t 'c)
  (check-false (dict-has-key? t 'c))

  (check-eq? 42 (get: (set: t 'c 42) 'c))

  ;; we can remove a key entry either with dict-remove! or by setting it to
  ;; undefined. We thus ensure that undefined could never appear as a value.
  (dict-remove! t 'c)
  (dict-set! t 'b undefined)
  (check-pred undefined? (t 'c))
  (check-pred undefined? (t 'b))

  (void (set-meta-table! t (table ('__index (table ('c 3))))))
  (check eq? 3 (t 'c))

  (void (set-meta-table! t (table ('__index (λ (self key) 42)))))
  (check eq? 1 (t 'a))
  (check eq? 42 (t 'c))

  (define/checked proxy-table (table))
  (void
   (set-meta-table! t (table ('__newindex proxy-table)))
   (lua-meta-table t)
   (dict-set! t 'd 42))
  (check-false (dict-has-key? t 'd))
  (check-true (dict-has-key? proxy-table 'd))

  (void
   (set-meta-table! t (table ('__newindex (λ (self key v) (rawset! self key 42)))))
   (dict-set! t 'foo 1))
  (check eq? 42 (t 'foo))

  (comment
   (check-false (hash-has-key? (lua-table t) 'd))
   ;; comment
   ))


;;* Inheritance ----------------------------------------------------- *;;


(module+ test

  (define/checked Account
    (table ('balance 0)
           ('withdraw (λ (self v) (set: self 'balance (- (get: self 'balance) v)) self))
           ('deposit  (λ (self v) (set: self 'balance (+ (get: self 'balance) v)) self))
           ('new (λ (self [o (table)])
                   (set: self '__index self)
                   (set-meta-table! o self)
                   o))))

  (test-case "invoke simple methods"

    ;; initial balance
    (check-eq? 0 (get: Account 'balance))

    ;; deposit
    (void ((get: Account 'deposit) Account 100))
    (check-eq? 100 (get: Account 'balance))

    ;; withdraw
    (void ((get: Account 'withdraw) Account 100))
    (check-eq? 0 (get: Account 'balance)))

  (define/checked LimitedAccount ((get: Account 'new) Account))
  (check-true (table? LimitedAccount))

  (define/checked s ((get: LimitedAccount 'new) LimitedAccount (table ('limit 1000))))

  (test-case "inherit from another table (class)"

    (check-eq? (get: s 'limit) 1000)
    (check-eq? (get: s 'balance) 0)

    (void ((get: s 'deposit) s 100))
    (check-eq? (get: s 'balance) 100)

    ;; withdraw
    (void ((get: s 'withdraw) s 100))
    (check-eq? (get: s 'balance) 0)

    (void (set: LimitedAccount 'get-limit (λ (self) (or (get: self 'limit) 0))))
    (check-eq? ((get: s 'get-limit) s) 1000))


  (test-case "override inherited method"

    ;; override withdraw method
    (void (set: LimitedAccount 'withdraw
                (λ (self v)
                  (if (> (- v (get: self 'balance)) ((get: self 'get-limit) self))
                      (error "insufficient funds")
                      (set: self 'balance (- (get: self 'balance) v)))
                  self)))

    (check-exn exn? (thunk ((get: s 'withdraw) s 1100)) "insufficient funds")
    (check-eq? (begin ((get: s 'withdraw) s 500) (get: s 'balance)) -500))

  (test-case "extend inheritance chain"

    (define/checked OverdraftAccount ((get: LimitedAccount 'new) LimitedAccount (table ('fee 5))))
    (check-true (table? OverdraftAccount))

    (define/checked d ((get: OverdraftAccount 'new) OverdraftAccount))
    (check-eq? (get: d 'fee) 5)

    (void (set: OverdraftAccount 'withdraw
                (λ (self v)
                  ;; TODO we should probably have a way to delegate to proto methods
                  ;; delegate to LimitedAccount.withdraw
                  ((get: (lua-meta-table (lua-meta-table self)) 'withdraw) self (+ (get: self 'fee) v)))))

    (check-eq? (get: d 'withdraw) (get: OverdraftAccount 'withdraw))
    (check-eq? (get: d 'withdraw) (get: (lua-meta-table d) 'withdraw))

    (check-exn exn? (thunk ((get: d 'withdraw) d 10)) "insufficient funds")
    (check-eq? (get: d 'balance) 0)

    (void (set: d 'limit 100))
    (void (checked ((get: d 'withdraw) d 10)))
    (check-eq? (get: d 'balance) -15)))


;;* Syntax ---------------------------------------------------------- *;;


(begin-for-syntax

  (define (table-colon-key? id)
    (match (symbol->string (syntax->datum id))
      ((regexp #rx"^(.+)\\:(.+)$" (list _ table key))
       (list (datum->syntax id (string->symbol table) id)
             (datum->syntax id (string->symbol key) id)))

      (else #f)))

  (define (table-dot-key? id)
    (match (symbol->string (syntax->datum id))
      ((regexp #rx"^(.+)\\.(.+)$" (list _ table key))
       (list
        (datum->syntax id (string->symbol table) id)
        (datum->syntax id (string->symbol key) id)))

      (else #f)))

  (define-syntax-class tck
    (pattern id:id
             #:when (table-colon-key? #'id)
             #:do [(match-define (list table key) (table-colon-key? #'id))]
             #:with table table
             #:with key key))

  (define-syntax-class tdk
    (pattern id:id
             #:when (table-dot-key? #'id)
             #:do [(match-define (list table key) (table-dot-key? #'id))]
             #:with table table
             #:with key key)))


;;** - #%app -------------------------------------------------------- *;;


(define-syntax (app stx)
  (syntax-parse stx

    ;; TODO check for correct syntax inside e
    ((_ e:expr ...)
     #:when (eq? #\{ (syntax-property stx 'paren-shape))
     #'(table e ...))

    ((_ f e ...)
     #'(#%app f e ...))))


;;** - #%top -------------------------------------------------------- *;;


(define-syntax (top stx)
  (syntax-parse stx

    ((_ . id:tdk) #'(get: id.table 'id.key))
    ;; TODO is it even reasonable to expand table:method in id position into a
    ;; curried function? First, we're being presumptuous, two, error gets reported
    ;; immediately rather than at the call site (maybe its good though).
    ((_ . id:tck) (syntax/loc stx
                    (let ((proc (get: id.table 'id.key)))
                      ;; TODO wait, would that check work for structs with
                      ;; proc:prop?
                      (unless (and (procedure? proc)
                                   (arity-includes?
                                    (procedure-arity (λ (_ . rest) _))
                                    (procedure-arity proc)))
                        (raise-result-error 'id "procedure of at least 1 argument" proc))
                      (curry (get: id.table 'id.key) id.table))))

    ((_ . id:id) #'(#%top . id))

    (_ (raise-syntax-error '#%top "invalid syntax in top"))))


(module+ test
  (define run-table-syntax-tests
    (dynamic-require "tests/test-table-syntax.rkt" 'run-tests))
  (run-table-syntax-tests))


;;** - define/table ------------------------------------------------- *;;


(define-syntax (define/table stx)
  (syntax-parse stx

    ;; TODO maybe guard these with (let ((t id.table)) (unless (table? t) error)),
    ;; or maybe even better if t is unbound, bind it to a fresh table! I wonder if
    ;; testing for bound is even possible? Worst case we could handle exn.

    ((_ id:tdk e:expr)
     ;; (define/table table.key val)
     #'(set: id.table 'id.key e))

    ((_ id:tck e:expr)
     ;; (define/table table:method proc)
     (syntax/loc stx
       (begin
         (define table:method e)
         (unless (and (procedure? table:method)
                      (arity-includes?
                       (procedure-arity (λ (_ . rest) _))
                       (procedure-arity table:method)))
           (raise-result-error 'id "procedure of at least 1 argument" table:method))
         (set: id.table 'id.key table:method))))

    ((_ (id:tdk arg ...) body:expr ...)
     ;; (define/table (table.key . args) body)
     (syntax/loc stx
       (begin
         (define (table.method arg ...) body ...)
         (set: id.table 'id.key table.method))))

    ((_ (id:tck arg ...) body:expr ...)
     ;; (define/table (table:method . args) body)
     #:with self (datum->syntax (car (syntax-e #'(body ...))) 'self)
     ;; TODO confirm self has the right scope and can be used in the body
     (syntax/loc stx
       (begin
         (define (table:method self arg ...) body ...)
         (set: id.table 'id.key table:method))))))


(module+ test

  (define/checked tbl (table))
  (void (checked (define/table tbl.key 42)))
  (void (checked (define/table (tbl.f arg) (+ 1 arg))))
  (void (checked (define/table (tbl:meth key) (get: self key))))
  (check-eq? ((get: tbl 'f) 42) 43)
  (check-eq? ((get: tbl 'meth) tbl 'key) 42)

  ;; NOTE unbound identifier is a compile time error that we can't just merily
  ;; catch at runtime
  (require syntax/macro-testing)
  (check-exn exn? (thunk (convert-compile-time-error
                          (define/table undefined-t.foo 42))))

  (define run-define/table-tests
    (dynamic-require "tests/test-table-syntax.rkt" 'run-define/table-tests))
  (run-define/table-tests))


;;* Notes -------------------------------------------------------- *;;


;; TODO Milestone 1: all examples in Lua book Ch20 and Ch21 must work correctly.
;; TODO Milestone 3: FastCGI in #lang racket/tables


;; TODO table as function should act like sending corresponding message to that
;; table:
;;
;; (table 'key . args) => ((get: table 'key) table . args)
;; (table proc . args) => (proc table . args)
;;   this one is tricky though and diverges, treat key-table specially?
;; (table key-table . args) => (key-table table . args) => repeat inf


;; TODO extended constructor
;;   {foo #:kw1 option
;;        #:kw2 option
;;        ('a 1)
;;        ('b 2)}


;; TODO Table must have identity a-la struct, so you could (a) predicate test for
;; it, (b) get it and maybe also the entire inheritance chain that should probably
;; also work for multiple inheritance. Should we call it meta-id or meta-type or
;; meta-class maybe?
;;
;; First we could you '__id metafield (or maybe even allow a function there). __id
;; would signal object-name, which we get by calling (object-name table) => simply
;; performs a lookup for __id returning undefined when missing. But that's not
;; all. On occasion we want a stricter check a-la eq? for object equality. What do
;; we do? Could the meta-table itself when present act as the other half of
;; table's identity?
;;
;; We say two tables have the same meta-id when first.__id = second.__id. We say
;; two tables have the same meta-type when they have the same meta-id, but also
;; their meta-tables are eq?. Are these definitions even useful?


;; TODO Equality and hashing. Or do structs already have a default hashing
;; function? We still want equal? implemented for tables IMO.


;; TODO How do you signal a missing table entry? #f, null, undefined don't quite
;; cut it out of the box since they are all first class values. Here's one option:
;; disallow undefined to ever appear as a value in a table. Obviously not possible
;; in hash-tables, structs etc, but since we control implementation of tables so
;; we can enforce it. Then undefined becomes special and returned as a result of a
;; lookup would signal an absense of value. We could even make it optional by
;; tweaking __newindex metamethod whenever such feature requested. But maybe it
;; ought to just be the default behavior. Alternative to flat out banning
;; undefined table values is to treat any key assignment to undefined as
;; dict-remove! Would that be cleaner?
;;
;; Incidentally this may actually work for any collection (e.g. vectors, lists,
;; etc) as long as we can control their get/set patterns.


;; TODO Redis backed tables (maybe other data structures, too). So access to a
;; redis backed table transparently handles its contents in Redis store rather
;; then Racket runtime only. I wonder what interesting semantics I might come up
;; with. Point is to have a completely transparent Racket API where Redis
;; interface doesn't even show unless say you want to serialize to disk etc.


;; TODO Tables as synchable events (maybe with embedded locks, semaphores,
;; mutexes). Would that make thread-safe concurrent programming more natural in
;; presence of tables? Worth an experiment.
