#lang prelude


(require racket/struct
         racket/generic
         (for-syntax syntax/parse
                     racket/match))


;;* Provides -------------------------------------------------------- *;;


(provide table table? set-meta-table! get-meta-table rawset! app top
         define/table
         tag? tag->string)


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
             ;; TODO I think this is a bug. Notice that this will traverse the
             ;; entire metatable chain to find __index. Or maybe that's the whole
             ;; point? How does Lua actually do that?
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


(define (get-meta-table t)
  (lua-meta-table t))


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

  ;; TODO do we need to check it doesn't already begin with : or :: ?
  (define (string->tag str)
    (string->symbol (format ":~a") str))


  (define (table-colon-key? id)
    (match (symbol->string (syntax->datum id))
      ((regexp #rx"^(.+)\\:(.+)$" (list _ table key))
       (list
        ;; table
        (datum->syntax id (string->symbol table) id)
        ;; tag key
        (datum->syntax id (string->symbol (format ":~a" key)) id)
        ;; sym key
        (datum->syntax id (string->symbol key) id)))

      (else #f)))

  (define (table-dot-key? id)
    (match (symbol->string (syntax->datum id))
      ((regexp #rx"^(.+)\\.(.+)$" (list _ table key))
       (list
        ;; table
        (datum->syntax id (string->symbol table) id)
        ;; tag key
        (datum->syntax id (string->symbol (format ":~a" key)) id)
        ;; sym key
        (datum->syntax id (string->symbol key) id)))

      (else #f)))

  (define-syntax-class tck
    (pattern id:id
             #:when (table-colon-key? #'id)
             #:do [(match-define (list table tag key) (table-colon-key? #'id))]
             #:with table table
             #:with key key
             #:with tag tag))

  (define-syntax-class tdk
    (pattern id:id
             #:when (table-dot-key? #'id)
             #:do [(match-define (list table tag key) (table-dot-key? #'id))]
             #:with table table
             #:with key key
             #:with tag tag))

  (define-syntax-class tag
    (pattern id:id
             #:when
             (regexp-match #px"^[:]\\S+" (symbol->string (syntax->datum #'id))))))


(define (tag? t)
  (and (symbol? t)
       (string-prefix? (symbol->string t) ":")))


(define/contract (tag->string t)
  (-> tag? string?)
  (symbol->string t))


;;** - #%app -------------------------------------------------------- *;;


(define-syntax (app stx)

  ;; TODO syntax validation and reporting probably belongs in table not the {}, or
  ;; maybe even in both places here and in table

  (define-syntax-class table-entry
    (pattern
     (~describe #:role "table entry"
                "(key value) pair"
                ((~and key:expr (~not (~literal quote))) value:expr))))

  (if (eq? #\{ (syntax-property stx 'paren-shape))

      ;; parse {}
      (syntax-parse stx
        #:context (list '|{ }| (with-syntax (((_ e ...) stx))
                                 (syntax/loc stx {e ...})))
        ((_ entry:table-entry ...)
         (syntax/loc stx (table entry ...))))

      ;; TODO this may actually be incorrect, e.g. (foo x . y) would simply fail
      ;; the below pattern-match. Either we need to allow such applications (Eli's
      ;; trick from Swindle) or better do (_ . rest) => (#%app . rest)

      ;; delegate to Racket's #:app
      (with-syntax (((_ f e ...) stx))
        (syntax/loc stx (#%app f e ...)))))


;;** - #%top -------------------------------------------------------- *;;


(define-syntax (top stx)
  (syntax-parse stx

    ;; TODO revisit my logic ops, this or here will return #f instead of
    ;; undefined, I may have been too hasty overloading or and etc
    ((_ . id:tdk) (syntax/loc stx (or (get: id.table 'id.tag)
                                      (get: id.table 'id.key))))

    ;; TODO ensure arity errors for methods generate meaningful errors

    ;; TODO maybe do procedure->method to report arity without self?

    ;; NOTE this clause runs every time table:method appears in code, whether in
    ;; application or identifier position. Also, note that if unbound identifier
    ;; occurs inside a list it'll be wrapped in #%top after the surrounding list
    ;; is wrapped in #%app. So, when we get here the receiver of the method (i.e.
    ;; the table) is known regardless of whether it is about to be called or
    ;; called later. To ensure the correct receiver is used when that happens we
    ;; essentially "curry" the original method with self bound to the receiver
    ;; (the table here). Two caveats:
    ;;
    ;; (a) we may not use curry no matter the method arity, this is because in
    ;; Racket curry is itself curried, that is:
    ;;
    ;;   ((curry proc)) = (curry proc)
    ;;
    ;;   so for example with method of arity > 1 (takes self and other args),
    ;;   immediate invocation as below will not report arity error, it'll simply
    ;;   return the same curried method. It'll keep doing that until the expected
    ;;   argument count is reached
    ;;
    ;;   ((curry method self)) => (curry method self)
    ;;
    ;; (b) we want to allow methods with keyword arguments, which means simple
    ;; curry and apply are out anyway. Luckily a combination of
    ;; make-keyword-procedure and keyword-apply appear to do the right thing
    ;; whether methods take keyword args or by position args alone.
    ((_ . id:tck) (syntax/loc stx
                    (let ((proc (or (get: id.table 'id.tag)
                                    (get: id.table 'id.key))))
                      ;; TODO wait, would that check work for structs with
                      ;; proc:prop?
                      (unless (and (procedure? proc)
                                   (arity-includes?
                                    (procedure-arity (λ (_ . rest) _))
                                    (procedure-arity proc)))
                        (raise-result-error 'id "procedure of at least 1 argument" proc))
                      (make-keyword-procedure
                       ;; methods that may take keyword args
                       (λ (kws kw-args . rest) (keyword-apply proc kws kw-args id.table rest))
                       ;; methods that only take by-position args
                       (λ args (apply proc id.table args))))))

    ;; wrap :tags in #%datum
    ((_ . id:tag) (syntax/loc stx (#%datum . id)))

    ((_ . id:id) (syntax/loc stx (#%top . id)))

    (_ (raise-syntax-error '#%top "invalid syntax in top"))))


;;** - define/table ------------------------------------------------- *;;


;; TODO support for nested (define/table ((foo.bar arg) arg) and such.


;; NOTE Attempt to define a key in a table-id that is unbound will report an
;; unbound identifier. This is in line with Lua but also seems more prudent. We
;; could get fancy and helpfully bind such identifier to a fresh table (see
;; `identifier-binding'), but I think attempt to deal with an unbound id is almost
;; always a bug somewhere.


(define-syntax (define/table stx)
  (syntax-parse stx

    ((_ id:tdk e:expr)
     ;; (define/table table.key val)
     (syntax/loc stx
       (begin
         ;; NOTE we introduce define here to ensure that
         ;; define/table is used where define is allowed
         (define table.key e)
         (set: id.table 'id.key table.key))))

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
                          (define/table undefined-t.foo 42)))))


(module+ test
  ;; NOTE Tests defined in prelude-tests/test-table-syntax

  ;; Since we cannot define table language tests here due to circularity, we
  ;; define and provide them as functions elsewhere and do this dynamic require
  ;; trick so we can run all table tests locally at dev time
  (define-syntax-rule (run-provided-tests #:in module test-thunk ...)
    (begin
      (define test-thunk (dynamic-require 'module 'test-thunk))
      ...
      (test-thunk)
      ...))

  (run-provided-tests #:in prelude-tests/test-lua-syntax
                      run-basic-table-tests
                      run-define/table-tests
                      run-simple-inheritance-tests
                      run-multiple-inheritance-tests
                      run-tags-tests))


;;* Reader ------------------------------------------------------- *;;


(module reader syntax/module-reader
  prelude/lua-lang)

;; TODO way to deal with . .. : :: operators in the reader

(comment
 (define parse-lp
   (case-lambda
     ((char port)
      ;; read
      (define ((default-read/prefix prefix) bytes-matched)
        (read/recursive #;port (input-port-append #t (open-input-string prefix) port)
                        #;char #\(
                        #;readtable #f))
      (cond
        ((regexp-try-match #px"^[.]\\s+"    port) => (default-read/prefix "send "))
        ((regexp-try-match #px"^[.][.]\\s+" port) => (default-read/prefix "sendmeta "))
        ((regexp-try-match #px"^[:]\\s+"    port) => (default-read/prefix "send/self "))
        ((regexp-try-match #px"^[:][:]\\s+" port) => (default-read/prefix "sendmeta/self "))
        (else (read/recursive port #\( #f))))
     ((char port src line col pos)
      ;; read-syntax
      (datum->syntax
       #f
       (parse-lp char port)
       ;; TODO I doubt this computes correct offset given the above send
       ;; replacements occur
       (let-values ([(l c p) (port-next-location port)])
         (list src line col pos (and pos (- p pos))))))))

 (parameterize ((current-readtable (make-readtable (current-readtable) #\( 'terminating-macro parse-lp))
                (current-input-port (open-input-string #<<eof
(begin
  (. foo 'meth a b)
  (.. foo 'meth a b)
  (: foo 'meth a b)
  (:: foo 'meth a b))
eof
)))
  (read))
;; comment
)


(comment
 (module reader syntax/module-reader
   prelude/tags-lang
   #:wrapper1 (λ (th)
                (parameterize ((current-readtable (readtable/tags)))
                  (th)))

   (define parse-tag ..body..)

   (define (readtable/tags)
     (make-readtable (current-readtable)
                     #\k 'dispatch-macro parse-tag)))
 ;; comment
 )


;;* Notes -------------------------------------------------------- *;;


;; TODO Milestone: MTP - Meta-table Protocol
;; TODO Milestone: FastCGI in #lang racket/tables


;; TODO Expose dot and colon identifier notation, so users may override it in
;; their lang/tables: we could wrap relevant syntax in #%.id #%:id #%..id #%::id.
;; Could push it even further and allow user-defined separators.

;; TODO candidates for pre-defined base methods that every table has access to:
;;
;; (t:meta) - get meta-table
;; (t:meta v) - set meta-table to v
;; (t:get key ...) - lookup key sequence
;; (t:set key ... v) - insert or update entry at key sequence to v
;; (t:send message . args) - call method aka send message with t as receiver
;;
;; IMO ideally these will have generic counterparts that simply dispatch on the
;; table by invoking its corresponding base method


;; TODO Table as function and table as method IMO could be implemented as generic
;; methods that by default delegate to corresponding metamethods __proc and
;; __method. This way we introduce flexibility at a higher level where you can
;; specialize the generics, at a lower level you could control behavior by
;; modifying relevant metamethods.


;; TODO Similarly, table lookup with get and modifying entries with set could be a
;; combination of generics and corresponding metamethods __get (__index) and
;; __set, with reasonable default behavior (standard metamethods).


;; TODO default table as function should act like sending corresponding message to
;; that table:
;;
;; (table 'key . args) => ((get: table 'key) table . args)
;; (table proc . args) => (proc table . args)
;;   this one is tricky though and diverges, treat key-table specially?
;; (table key-table . args) => (key-table table . args) => repeat inf


;; TODO extended constructor - amounts to instantiating from foo metatable, that
;; aka (foo:new #:kw1 option #:kw2 option {('a 1) ('b 2)}])
;;
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
