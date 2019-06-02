#lang prelude


(require racket/struct
         racket/generic
         (for-syntax syntax/parse
                     racket/match)
         (only-in racket
                  [#%top racket/#%top]
                  [#%app racket/#%app]))


(provide (rename-out (#%top top) (#%app app)))


(module+ test
  (require rackunit)
  (require prelude/testing))


;;* :tags -------------------------------------------------------- *;;


(begin-for-syntax
  (define-syntax-class tag
    (pattern id:id
             #:when
             (regexp-match #px"^[:]\\S+" (symbol->string (syntax->datum #'id)))))

  (define (table-sep-key? id sep)
    (define rx (format "^(.+)~a(.+)$" (regexp-quote sep)))
    (match (symbol->string (syntax->datum id))
      ((regexp rx (list _ table key))
       (list
        ;; table
        (datum->syntax id (string->symbol table) id)
        ;; tag key
        (datum->syntax id (string->symbol (format ":~a" key)) id)
        ;; sym key
        (datum->syntax id (string->symbol key) id)))
      (else #f)))

  ;; TODO dry this - parameterizing by sep
  (define-syntax-class table-colon-key
    (pattern id:id
             #:when (table-sep-key? #'id ":")
             #:do [(match-define (list table tag key) (table-sep-key? #'id ":"))]
             #:with table table
             #:with key key
             #:with tag tag))

  (define-syntax-class table-dot-key
    (pattern id:id
             #:when (table-sep-key? #'id ".")
             #:do [(match-define (list table tag key) (table-sep-key? #'id "."))]
             #:with table table
             #:with key key
             #:with tag tag)))

(define (tag? t)
  (and (symbol? t)
       (string-prefix? (symbol->string t) ":")))


;;* #%top -------------------------------------------------------- *;;


(define-syntax (#%top stx)
  (syntax-parse stx

    ((_ . id:table-dot-key)
     ;; table.key =>
     (syntax/loc stx (or? (get id.table 'id.tag)
                          (get id.table 'id.key))))

    ((_ . id:table-colon-key)
     ;; table:key =>
     (syntax/loc stx
       (let ((proc (or? (get id.table 'id.tag)
                        (get id.table 'id.key))))
         (make-keyword-procedure
          (λ (kws kw-args . rest) (keyword-apply proc kws kw-args id.table rest))
          (λ args (apply proc id.table args))))))

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


(define (meta-get t key)
  (define mt (table-meta t))
  (and? mt (dict-ref mt key)))


(define table-procedure
  (make-keyword-procedure
   (λ (kws kw-args t . rest)
     (let ((proc (meta-get t :<proc>)))
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

   (define (dict-ref t key [default (λ () undefined)]) (ref (table-dict t) key default))
   (define dict-set!          (redirect-generic set!))
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


;;* <get> metamethod --------------------------------------------- *;;


;; TODO Default table struct constructor doesn't check the values in the table, so
;; it will happily allow undefined their. We'll have to disallow undefined in
;; whatever constructor we end up providing. We probably want this as the final
;; step after any calls to <setmeta> to ensure user doesn't accidentally sets to
;; undefined in the post-creation <setmeta> step.


;; TODO consider optional default proc argument?
(define (get t key)
  (if (dict-has-key? t key)
      (dict-ref t key)
      (if (table? (table-meta t))
          (let* ((mt (table-meta t))
                 (metamethod (dict-ref mt :<get>)))
            (if? metamethod
                 (cond
                   ((table? metamethod) (get metamethod key))
                   ((procedure? metamethod) (metamethod t key))
                   ;; TODO should error here instead?
                   (else undefined))
                 (get mt key)))
          undefined)))


;;* <set> metamethod --------------------------------------------- *;;


(define (set t key v)
  (if (undefined? v)
      (raise-argument-error 'set "not undefined?" 3 v t key)
      (if (dict-has-key? t key)
          (dict-set! t key v)
          (if (table? (table-meta t))
              (let* ((mt (table-meta t))
                     (metamethod (dict-ref mt :<insert>)))
                (cond
                  ((table? metamethod) (set metamethod key v))
                  ((procedure? metamethod) (metamethod t key v))
                  ;; TODO should we signal an error?
                  (else (dict-set! t key v))))
              (dict-set! t key v))))
  t)


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

  (test-case "set: with no <insert> metamethod"
    ;; insert
    (check-not-exn (thunk (set t :c 3)))
    (check-eq? (get t :c) 3)
    ;; update
    (check-not-exn (thunk (set t :a 0)))
    (check-eq? (get t :a) 0))

  (test-case "set: when <insert> metamethod is a table"
    (check-not-exn (thunk (set mt :<insert> mt)))
    ;; insert
    (check-not-exn (thunk (set t :d 4)))
    (check-eq? (get mt :d) 4)
    (check-eq? (get t :d) 4)
    ;; update inserted
    (check-not-exn (thunk (set t :d 0)))
    (check-eq? (get mt :d) 0)
    ;; update existing
    (check-not-exn (thunk (set t :a -1)))
    (check-eq? (get t :a) -1))

  (test-case "set: when <insert> metamethod is a procedure"
    (check-not-exn (thunk (set mt :<insert> (λ (_ k v) (set mt k v)))))
    (check-not-exn (thunk (set t :e 5)))
    (check-eq? (get mt :e) 5)
    (check-eq? (get t :e) 5)))


;;* <hierarchy> -------------------------------------------------- *;;


(define <table> (table (ht) undefined))


;;* #%app -------------------------------------------------------- *;;


(begin-for-syntax

  ;; TODO this does not cover #:kw arguments but we should capture them and simply
  ;; pass  along to #%table constructor.
  (define-syntax-class table-entry
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
     (syntax/loc stx (let* ((t (table (ht entry ...) mt))
                            (metamethod (or? (meta-get t :<setmeta>) identity)))
                       (metamethod t))))))


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

  (test-case "Default table constructor invokes <setmeta>"
    (define/checked c {<c> (:a 1)})
    (check-eq? (get c :a) 1)
    (check-eq? (get c :answer) 42)
    (check-eq? (dict-ref c :answer) 42))

  (test-case "Use #%table from macro invocation context"
    (let-syntax ([#%table (syntax-rules () [(_ mt entry ...) (ht entry ...)])])
      (check-equal? (ht (:a 1)) {(:a 1)}))))