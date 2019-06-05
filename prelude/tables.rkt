#lang prelude


(require racket/struct
         racket/generic
         syntax/parse/define
         (for-syntax syntax/parse
                     racket/match)
         (only-in racket
                  [#%top racket/#%top]
                  [#%app racket/#%app]))


(provide #%top #%app #%table #%.
         get set meta-dict-ref
         <table>
         tag?)


(module+ test
  (require rackunit)
  (require prelude/testing))


;;* :tags -------------------------------------------------------- *;;


(begin-for-syntax
  (define-syntax-class tag
    (pattern id:id
             #:when
             (regexp-match #px"^[:]\\S+" (symbol->string (syntax->datum #'id))))))


(define (tag? t)
  (and (symbol? t)
       (string-prefix? (symbol->string t) ":")))


;;* #%. ---------------------------------------------------------- *;;


(begin-for-syntax

  (define (table-sep-key? id . seps)
    (define (rx sep) (format "^(.+)~a(.+)$" (regexp-quote sep)))
    (define idstr (symbol->string (syntax->datum id)))
    (for/or ((sep (in-list seps)))
      (match idstr
        ((regexp (rx sep) (list _ table key))
         ;; => (list "sep" "table" "key")
         (list sep table key))
        (else #f))))

  (define-syntax-class (table-sep-key sep)
    (pattern id:id
             #:attr split (table-sep-key? #'id sep)
             #:when (attribute split)
             #:do [(match-define (list sep table key) (attribute split))]
             #:with table (datum->syntax #'id (string->symbol table) #'id)
             #:with tag   (datum->syntax #'id (string->symbol (format ":~a" key)) #'id)
             #:with sym   (datum->syntax #'id (string->symbol key) #'id))))


(define-syntax-parser #%.
  ((_ "." (~var id (table-sep-key ".")))
   (syntax/loc #'id (or? (get id.table 'id.tag)
                         (get id.table 'id.sym))))
  ((_ ":" (~var id (table-sep-key ":")))
   (syntax/loc #'id
     (let ((proc (or? (get id.table 'id.tag)
                      (get id.table 'id.sym))))
       (unless (procedure? proc)
         (raise-result-error 'id "procedure?" proc))
       (make-keyword-procedure
        (λ (kws kw-args . rest) (keyword-apply proc kws kw-args id.table rest))
        (λ args (apply proc id.table args)))))))


;;* #%top -------------------------------------------------------- *;;


(define-syntax (#%top stx)
  (syntax-parse stx

    ((_ . id:id)
     ;; table.key =>
     #:attr split (table-sep-key? #'id "." ":")
     #:when (attribute split)
     #:with sep (car (attribute split))
     #:with #%. (datum->syntax #'id '#%. #'id)
     (syntax/loc #'id (#%. sep id)))

    ((_ . id:tag)
     ;; :tag =>
     (syntax/loc stx (#%datum . id)))

    ((_ . id:id)
     ;; id =>
     (syntax/loc stx (racket/#%top . id)))

    (_ (raise-syntax-error '#%top "invalid syntax in top"))))


(module+ test
  (test-case "t.k and t:k accessors"
    (define <proc> {(:<proc> (λ (mt t k) (get t k)))})
    (define proc {<proc>})
    (define t {(:a 1)
               ;; procedure
               (:f (λ (t k) (get t k)))
               ;; struct procedure
               (:proc proc)})
    (check-eq? t.a 1)
    (check-eq? (t.f t :a) 1)
    (check-eq? (t:f :a) 1)
    (check-eq? (t.proc t :a) 1)
    (check-eq? (t:proc :a) 1)))


;;* table struct ------------------------------------------------- *;;


;; NOTE Replace apply with keyword-apply if some dict method takes kw args
(define ((redirect-generic generic) t . args)
  (apply generic (table-dict t) args))


(define (meta-dict-ref t key)
  (define mt (table-meta t))
  (if (table? mt) (dict-ref mt key) undefined))


(define table-procedure
  (make-keyword-procedure
   (λ (kws kw-args t . rest)
     (let ((proc (meta-dict-ref t :<proc>)))
       (if (procedure? proc)
           ;; NOTE t here will always be bound to the table that was invoked as
           ;; procedure i.e. table whose metatable specifies (:<proc> proc) - a
           ;; consequence of how Racket prop:procedure operates. This means that
           ;; proc must be a procedure of at least one argument.
           (keyword-apply proc kws kw-args t rest)
           (error "table has no <proc> metamethod to apply"))))))


(struct table (dict meta)
  #:mutable
  ;; NOTE We make table struct #:transparent for now to avoid the need for custom
  ;; gen:equal+hash interface. Basically, we fallback on the default equal? Would
  ;; need to revisit if this becomes a perf bottleneck or table semantics changes.
  #:transparent

  #:property prop:procedure table-procedure

  #:methods gen:dict
  ((define/generic ref           dict-ref)
   (define/generic set!          dict-set!)
   (define/generic has-key?      dict-has-key?)
   (define/generic remove!       dict-remove!)
   (define/generic iterate-first dict-iterate-first)
   (define/generic iterate-next  dict-iterate-next)
   (define/generic iterate-key   dict-iterate-key)
   (define/generic iterate-value dict-iterate-value)

   (define (dict-ref t k [default (λ () undefined)])
     (ref (table-dict t) k default))

   (define (dict-set! t k v)
     (define dict (table-dict t))
     (if (undefined? v) (remove! dict k) (set! dict k v)))

   (define dict-has-key?      (redirect-generic has-key?))
   (define dict-remove!       (redirect-generic remove!))
   (define dict-iterate-first (redirect-generic iterate-first))
   (define dict-iterate-next  (redirect-generic iterate-next))
   (define dict-iterate-key   (redirect-generic iterate-key))
   (define dict-iterate-value (redirect-generic iterate-value)))

  #:methods gen:custom-write
  ((define write-proc
     (make-constructor-style-printer
      (λ (t) 'table)
      (λ (t) (for/list (((k v) (in-dict (table-dict t))))
               (list k v)))))))


(module+ test

  (test-case "<proc>"
    (define <proc> (table (ht (:<proc> dict-ref)) undefined))
    (define <kwproc> (table (ht (:<proc> (λ (t #:key key) (dict-ref t key)))) undefined))
    (check-eq? ((table (ht (:a 1)) <proc>) :a) 1)
    (check-eq? ((table (ht (:a 1)) <kwproc>) #:key :a) 1)
    (check-exn exn? (thunk ((table (ht) undefined) 1)) "table has no <proc>"))

  (test-case "equality"
    (define <mt> {(:b 2)})
    (check-true (equal? {} {}))
    (check-true (equal? {<mt> (:a 1)} {<mt> (:a 1)}))
    (check-false (equal? {(:a 1)} {}))))


(module+ test

  (test-case "gen:dict"
    (define t (table (ht (:a 1)) undefined))
    (check-true (dict-has-key? t :a))
    (check-eq? (dict-ref t :a) 1)
    (check-true (undefined? (dict-ref t :b)))
    (check-not-exn (thunk (dict-set! t :b 2)))
    (check-eq? (dict-ref t :b) 2))

  (test-case "gen:associative"
    (define tb (table (ht (:b 1)) undefined))
    (define ta (table (ht (:a tb)) undefined))
    (check-eq? (get: ta :a :b) 1)
    (check-not-exn (thunk (set: ta :a :c 2)))
    (check-eq? (get: ta :a :c) 2)))


;;* get ---------------------------------------------------------- *;;


(define (get t k)
  (if (dict-has-key? t k)
      (dict-ref t k)
      (let ((mt (table-meta t))
            ;; we rely on meta-dict-ref returning undefined when mt is not a table
            (metamethod (meta-dict-ref t :<get>)))
        (cond
          ((table? metamethod) (get metamethod k))
          ((procedure? metamethod) (metamethod t k))
          ((undefined? metamethod) (if (table? mt) (get mt k) undefined))
          (else (raise-argument-error '<get> "table or procedure" metamethod))))))


;;* set ---------------------------------------------------------- *;;


;; NOTE set : (table? k v . -> . table?) which means <set> metamethod must comply
;; with that contract
(define (set t k v)
  (define metamethod (meta-dict-ref t :<set>))
  (cond
    ;; TODO is this too much, should we dict-set! to cut recursion?
    ((table? metamethod) (set metamethod k v))
    ((procedure? metamethod) (metamethod t k v))
    ((undefined? metamethod) (dict-set! t k v) t)
    (else (raise-argument-error '<set> "table or procedure" metamethod))))


(define (rm t k) (set t k undefined))


(module+ test

  (define/checked mt (table (ht (:b 2)) undefined))
  (define/checked t (table #;t (ht (:a 1)) #;mt mt))
  (define/checked tt (table #;t (ht) #;mt t))
  (define/checked t<get>proc (table #;t (ht) #;mt (table (ht (:<get> (λ (_ key) (get t key)))) undefined)))
  (define/checked t<get>table (table #;t (ht) #;mt (table (ht (:<get> t)) undefined)))

  (test-case "get: when mt is a table"
    (check-eq? (get t :a) 1)
    (check-eq? (get t :b) 2)
    (check-pred undefined? (get t :c))
    ;; deeper mt chain
    (check-eq? (get tt :a) 1)
    (check-eq? (get tt :b) 2)
    (check-pred undefined? (get tt :c)))

  (test-case "get: when <get> metamethod is a procedure"
    (check-eq? (get t<get>proc :a) 1)
    (check-eq? (get t<get>proc :b) 2)
    (check-pred undefined? (get t<get>proc :c)))

  (test-case "get: when <get> metamethod is a table"
    (check-eq? (get t<get>table :a) 1)
    (check-eq? (get t<get>table :b) 2)
    (check-pred undefined? (get t<get>table :c)))

  (test-case "set: with no <set> metamethod"
    ;; insert
    (check-not-exn (thunk (set t :c 3)))
    (check-eq? (get t :c) 3)
    ;; update
    (check-not-exn (thunk (set t :a 0)))
    (check-eq? (get t :a) 0))

  (test-case "set: when <set> is a table"
    (check-not-exn (thunk (set mt :<set> mt)))
    ;; insert
    (check-not-exn (thunk (set t :d 4)))
    (check-eq? (get mt :d) 4)
    (check-eq? (get t :d) 4)
    ;; update inserted
    (check-not-exn (thunk (set t :d 0)))
    (check-eq? (get mt :d) 0)
    ;; update existing
    (check-not-exn (thunk (set t :a -1)))
    (check-eq? (get t :a) 0)
    (check-eq? (get mt :a) -1))

  (test-case "set: when <set> is a procedure"
    (check-not-exn (thunk (set mt :<set> (λ (_ k v) (set mt k v)))))
    (check-not-exn (thunk (set t :e 5)))
    (check-eq? (get mt :e) 5)
    (check-eq? (get t :e) 5)))


;;* <hierarchy> -------------------------------------------------- *;;


(define <table> (table (ht) undefined))


;;* #%table ------------------------------------------------------ *;;


(begin-for-syntax

  ;; TODO this does not cover #:kw arguments but we should capture them and simply
  ;; pass  along to #%table constructor.
  (define-syntax-class table-entry
    #:attributes (key value)
    (pattern
     (~describe #:role "table entry"
                "(key value) pair"
                ((~and key:expr (~not (~literal quote))) value:expr)))))


(define-syntax (#%table stx)
  (syntax-parse stx
    #:context (list '|{}| (with-syntax (((_ e ...) stx))
                            ;; doesn't seem to effect paren shape in error msg
                            (syntax-property (syntax/loc stx {e ...}) 'paren-shape #\{)))
    ((_ mt:id entry:table-entry ...)
     (syntax/loc stx (let* ((h (ht entry ...))
                            (t (table h mt))
                            (metamethod (or? (meta-dict-ref t :<setmeta>) identity))
                            (undefs (for/list (((k v) (in-mutable-hash h))
                                               #:when (undefined? v))
                                      k)))
                       (for-each (curry dict-remove! h) undefs)
                       ;; Or ok according to docs to drop keys like this:
                       ;; (hash-map h (λ (k _) (dict-remove! h k)))
                       (metamethod t))))))


;;* #%app -------------------------------------------------------- *;;


(define-syntax (#%app stx)
  (if (eq? #\{ (syntax-property stx 'paren-shape))

      ;; parse {#%app}
      (syntax-parse stx

        ;; TODO would it make sense to use <table> binding at the call site?
        ;; Thereby allowing the user to swap it for something else? Beware
        ;; accidentally making <table> dynamically scoped though? I think the same
        ;; trick as with #%table would work here.
        ((_ (~optional (~seq mt:id) #:defaults ((mt #'<table>))) e ...)
         #:with #%table (datum->syntax stx '#%table stx)
         (syntax/loc stx (#%table mt e ...)))

        ;; NOTE we use dotted pair to match to correctly cover application
        ;; expressions that may be using dot-notation themselves e.g. (foo x . y)
        ((_ . rest)
         ;; delegate to Racket #%app
         (syntax/loc stx (racket/#%app . rest))))

      ;; parse non {#%app}
      (with-syntax (((_ . rest) stx))
        ;; delegate to Racket's #:app
        (syntax/loc stx (racket/#%app . rest)))))


(module+ test
  (define/checked <c> {(:<setmeta> (λ (t) (set t :answer 42)))})

  (test-case "undefined values"
    (define t {(:a 1) (:b 2)})
    (check-equal? {(:a 1) (:c undefined) (:b 2) (:d undefined)} t)
    (check-equal? (set t :c undefined) t)
    (check-equal? (set t :b undefined) {(:a 1)})
    (check-equal? (rm t :a) {}))

  (test-case "Default table constructor invokes <setmeta>"
    (define/checked c {<c> (:a 1)})
    (check-eq? (get c :a) 1)
    (check-eq? (get c :answer) 42)
    (check-eq? (dict-ref c :answer) 42))

  (test-case "Use #%table from macro invocation context"
    (let-syntax ([#%table (syntax-rules () [(_ mt entry ...) (ht entry ...)])])
      (check-equal? (ht (:a 1)) {(:a 1)}))))


;;* <spec> ------------------------------------------------------- *;;


;; TODO better error reports needed: k, v, predicate that failed. I bet contracts
;; RHS should be able to capture which subcontract failed exactly, but I dunno
;; how that works. Ditto contract error reporting facilities.


(define <spec>
  {(:<proc> (case-lambda
              ((spec t)
               (for (((k pred?) (in-dict spec)))
                 ;; TODO should this be get instead of dict-ref?
                 (unless (pred? (dict-ref t k))
                   (error '#%table "slot ~a violated its contract" k)))
               t)
              ((spec t k v)
               (define pred? (or? (dict-ref spec k) (const #t)))
               (unless (pred? v)
                 (error 'set "slot ~a violated its contract" k))
               t)))})


(define <only>
  {(:<proc> (case-lambda
              ((spec t)
               (define slots (list->mutable-seteq (dict-keys t)))
               (for (((k pred?) (in-dict spec)))
                 (unless (pred? (dict-ref t k))
                   (error '#%table "slot ~a violated its contract" k))
                 (set-remove! slots k))
               (unless (set-empty? slots)
                 (error '#%table "slots ~a not allowed by <spec>" (set->list slots)))
               t)
              ((spec t k v)
               (define pred? (or? (dict-ref spec k)
                                  (const (error 'set "slot ~a not allowed by <spec>" k))))
               (unless (pred? v)
                 (error 'set "slot ~a violated its contract" k))
               t)))})


(module+ test

  (test-case "<spec>"
    (define/checked <mt> {(:check {<spec> (:a (or/c undefined? natural?))
                                          (:b (or/c undefined? symbol?))
                                          (:c symbol?)})
                          (:<setmeta> (λ (t) (t:check)))
                          (:<set> (λ (t k v) (t:check k v) (dict-set! t k v) t))})
    (define/checked t {<mt> (:a 1) (:c 'c)})
    ;; :c must be present
    (check-exn exn? (thunk {<mt>}) "slot :c violated its contract")
    ;; :c and :b must be symbols
    (check-exn exn? (thunk {<mt> (:c 42)}) "slot :c violated its contract")
    (check-exn exn? (thunk (set t :c 42))  "slot :c violated its contract")
    (check-exn exn? (thunk (set t :b 42))  "slot :c violated its contract")
    ;; happy path
    (check-eq? t.c 'c)
    (check-eq? (get (set t :b 'b) :b) 'b))


  (test-case "<only>"
    (define/checked <mt> {(:check {<only> (:a (or/c undefined? natural?))
                                          (:b (or/c undefined? symbol?))
                                          (:c symbol?)})
                          (:<setmeta> (λ (t) (t:check)))
                          (:<set> (λ (t k v) (t:check k v) (dict-set! t k v) t))})
    (define/checked t {<mt> (:a 1) (:c 'c)})
    ;; only speced slots allowed
    (check-exn exn? (thunk {<mt> (:a 1) (:d 4)}) "slots (:d) not allowed")
    (check-exn exn? (thunk (set t :d 4)) "slot :d not allowed")
    ;; otherwise like <spec>
    (check-exn exn? (thunk {<mt>}) "slot :c violated its contract")
    (check-exn exn? (thunk {<mt> (:c 42)}) "slot :c violated its contract")
    (check-exn exn? (thunk (set t :c 42))  "slot :c violated its contract")
    (check-exn exn? (thunk (set t :b 42))  "slot :c violated its contract")
    (check-eq? t.c 'c)
    (check-eq? (get (set t :b 'b) :b) 'b)))
