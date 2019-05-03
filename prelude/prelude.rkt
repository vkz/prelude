#lang racket


(provide comment
         example
         undefined?
         some?
         none?
         or?
         and?
         if?
         when?
         associative-on-key-missing
         gen:associative
         get: set:
         ht-equality
         ht-immutable
         ht hti ht*
         ~>)


(require (for-syntax syntax/parse syntax/keyword racket/match)
         racket/generic
         racket/undefined)


(define-syntax-rule (comment . any) (void))
(define-syntax-rule (example . any) (void))
(begin-for-syntax
  (define-syntax-rule (comment . any) (void))
  (define-syntax-rule (example . any) (void)))


(module+ test
  (require rackunit))


(define (undefined? e)
  (eq? undefined e))


(define (some? val)
  ;; should I also treat null as #f?
  (if (undefined? val) #f val))


(define (none? val)
  (not (some? val)))


(define-syntax-rule (or? e ...)
  (or (some? e) ...))


(define-syntax-rule (and? e ...)
  (and (some? e) ...))


(define-syntax-rule (if? test then else)
  (if (some? test) then else))


(define-syntax-rule (when? test body ...)
  (when (some? test) body ...))


;; TODO cond?
;; TODO when-let, if-let


(module+ test

  (define v (vector 'a 'b))
  (define h (hash 'a 1 'b v))
  (define h! (make-hash `((a . 1) (b . ,v))))
  (define alist '((a . 1) (b . 2))))


(define associative-on-key-missing (make-parameter make-hash))


(define (alist? h)
  (and (list? h) (andmap pair? h)))


(define-generics associative
  (assoc-get associative key [default])
  (assoc-set associative key val)
  #:fast-defaults ((alist?
                    (define (assoc-get table key [default (lambda () (error "key not found" key))])
                      (dict-ref table key default))
                    (define (assoc-set table key val)
                      (dict-set table key val)))
                   (dict?
                    ;; TODO should I return undefined on key missing instead of
                    ;; throwing like dict does?
                    ;; (define assoc-get dict-ref)
                    (define (assoc-get table key [default (lambda () (error "key not found" key))])
                      (dict-ref table key default))
                    (define (assoc-set table key val)
                      (if (immutable? table)
                          (dict-set table key val)
                          (begin
                            (dict-set! table key val)
                            table))))))


(define (get: #:default [default (thunk undefined)] table key . keys)
  (unless (associative? table)
    (error "Expected associative"))
  (if (empty? keys)
      (assoc-get table key default)
      (keyword-apply get: '(#:default) (list default) (assoc-get table key) keys)))


(define set:
  (case-lambda
    ((table key val) (assoc-set table key val))
    ((table key next-key . more)
     (assoc-set table key (apply set: (assoc-get table key (associative-on-key-missing)) next-key more)))))

;; TODO rm: or remove: that removes key-value pair where possible. Should remove
;; in hash-tables and alists, do nothing for structs.

(module+ test

  (check eq? 'b (get: h 'b 1))

  (check eq? 42 (get: (set: h 'b 0 42) 'b 0))

  ;; mutable hash-table
  (void (set: h! 'c 'd 'e 42))
  (check eq? 42 (get: h! 'c 'd 'e))
  (check-false (immutable? (get: h! 'c 'd)))

  ;; immutable hash-table
  (parameterize ((associative-on-key-missing hash))
    (set! h (set: h 'c 'd 'e 42)))
  (check eq? 42 (get: h 'c 'd 'e))
  (check-pred immutable? (get: h 'c 'd))

  ;; alists
  (set! alist (set: alist 'b 42))
  (check-pred alist? alist)
  (check-eq? 42 (get: alist 'b))
  (check-eq? 42 (get: (set: alist 'c 'd 'e 42) 'c 'd 'e))
  ;; yep, we can totally make nested alists
  (parameterize ((associative-on-key-missing list))
    (check-eq? 42 (get: (set: alist 'c 'd 'e 42) 'c 'd 'e)))

  ;; missing keys

  ;; default missing to undefined
  (check eq? undefined (get: (ht) 'a))
  (check eq? undefined (get: (ht ('a (ht))) 'a 'b))

  ;; allow custom missing key action
  (check-exn exn:fail? (thunk (get: (ht) 'a #:default (thunk (error "not found")))))
  (check-exn exn:fail? (thunk (get: (ht ('a (ht))) 'a 'b #:default (thunk (error "not found"))))))

(comment

 ;; TODO #:associative struct prop
 (struct foo (v) #:associative)
 ;; =>
 (struct foo (v)
   #:methods gen:associative
   ((define (assoc-get))
    (define (assoc-set))))

 ;; Actually, we should implement gen:dict interface instead, then get: and set:
 ;; would hopefully just work without my changing any of the gen:associative code
 ;; and all dict methods would work, too!

 ;; comment
 )


(define ht-equality (make-parameter equal?))
(define ht-immutable (make-parameter false))


(define (ht-constructor)

  (define ((op-is? eq) op)
    (eq? eq op))

  (match (list (ht-equality) (ht-immutable))

    ((list (? (op-is? eq?)) #t) make-immutable-hasheq)
    ((list (? (op-is? eq?)) #f) make-hasheq)

    ((list (? (op-is? eqv?)) #t) make-immutable-hasheqv)
    ((list (? (op-is? eqv?)) #f) make-hasheqv)

    ((list (? (op-is? equal?)) #t) make-immutable-hash)
    ((list (? (op-is? equal?)) #f) make-hash)))


(begin-for-syntax

  ;; NOTE We want to be able to use the same name _ht_ to both construct
  ;; hash-tables with inner key-value pairs implicitly quoted and match
  ;; hash-tables inside match. So, ht needs to be a macro as well as a
  ;; match-expander of the same name. The only way to prevent naming conflict is
  ;; to bind ht with define-syntax to the following struct. It's the trick I
  ;; learnt from docs for prop:pattern-expander which lets you extend syntax-parse
  ;; with a new pattern, so e.g. by adding this prop to ht-expander I could define
  ;; a new pattern for syntax-parse but I won't.
  (struct ht-expander (expand match-expand)
    #:property prop:procedure 0
    #:property prop:match-expander 1)

  (define ht-expand
    (syntax-parser
      ;; with equality
      ((_ #:eq op:expr (key:expr value:expr) ...)
       #'(parameterize ((ht-equality op)) ((ht-constructor) `((,key . ,value) ...))))

      ((_ (key:expr value:expr) ... #:eq op:expr)
       #'(parameterize ((ht-equality op)) ((ht-constructor) `((,key . ,value) ...))))

      ;; with default equality
      ((_ (key:expr value:expr) ...)
       #'((ht-constructor) `((,key . ,value) ...)))))

  ;; NOTE We extend Racket match with two table patterns ht - strict pattern where
  ;; every sub-pattern must match, and ht* - permissive pattern where any missing
  ;; keys would simply be bound to undefined. Both support final repeat ...
  ;; pattern that would match remaining table entries. Both work for hash-tables
  ;; and alists.

  ;; NOTE basic idea for ht match-expander
  (example
   (match (dict->list (ht ('a 1) ('b 2)))
     ((hash-table ('b v)) v)
     ((list-no-order (cons 'b vb) (cons 'a va) _ ...) (list va vb)))
   ;; example
   )

  (define-syntax-class ht
    (pattern ((~datum quote) var:id) #:with key this-syntax #:attr ignore #f)
    (pattern (key var) #:attr ignore #f)
    (pattern (~literal _) #:with key #f #:with var #f #:attr ignore #t)
    (pattern var:id #:with key (datum->syntax #'var `',(syntax-e #'var)) #:attr ignore #t))

  (define ht-match-expand
    (syntax-parser

      ;; allow final repeating pattern: (keypat valpat) ...
      ((_ ht:ht ... (~seq htlast:ht (~literal ...)))
       #:with ht...    (if (attribute htlast) #'(htlast (... ...)) #'())
       #:with alist... (if (attribute htlast)
                           (if (attribute htlast.ignore)
                               #'(htlast (... ...))
                               #'((cons htlast.key htlast.var) (... ...)))
                           #'())
       ;; TODO to stay true to types that implement dict interface this matcher
       ;; should have another branch to somehow work for vectors but I wonder if
       ;; that'd be at all useful.
       #`(or (hash-table (ht.key ht.var) ... #,@#'ht...)
             (list-no-order (cons ht.key ht.var) ... #,@#'alist...)))

      ;; only key val patterns
      ((_ ht:ht ...)
       #`(or (hash-table (ht.key ht.var) ...)
             (list-no-order (cons ht.key ht.var) ... _ (... ...)))))))


;; Syntax: mutable hash-table constructor that doubles as a match pattern of the
;; same name.
(define-syntax ht (ht-expander ht-expand ht-match-expand))


;; Syntax: immutable hash-table constructor just for completeness
(define-syntax (hti stx)
  (syntax-parse stx
    ;; with equality
    ((_ #:eq op:expr (key:expr value:expr) ...)
     #'(parameterize ((ht-equality op) (ht-immutable true)) ((ht-constructor) `((,key . ,value) ...))))

    ((_ (key:expr value:expr) ... #:eq op:expr)
     #'(parameterize ((ht-equality op) (ht-immutable true)) ((ht-constructor) `((,key . ,value) ...))))

    ;; with default equality
    ((_ (key:expr value:expr) ...)
     #'(parameterize ((ht-immutable true)) ((ht-constructor) `((,key . ,value) ...))))))


(module+ test
  (test-case "ht and hti constructors"

    (let ((key "key"))
      (check-eq? 42 (get: (ht (key 1) ('nested (ht ('foo 42))) #:eq eqv?) 'nested 'foo)))

    (let ((h (ht ('type 1) ('tag 42))))
      (check-false (immutable? h))
      (check-pred hash-equal? h))

    (let ((h (ht ('type 1) ('tag 42) #:eq eqv?)))
      (check-false (immutable? h))
      (check-pred hash-eqv? h))

    (let ((h (ht #:eq eq? ('type 1) ('tag 42))))
      (check-false (immutable? h))
      (check-pred hash-eq? h))

    (let ((h (hti #:eq eq? ('type 1) ('tag 42))))
      (check-pred immutable? h)
      (check-pred hash-eq? h))

    (parameterize ((ht-immutable true)
                   (ht-equality eqv?))
      (let ((h (ht ('type 1) ('tag 42))))
        (check-pred immutable? h)
        (check-pred hash-eqv? h)))))


(module+ test
  (test-case "match with ht-pattern"

    ;; hash-tables
    (check equal? '(1 2 3) (match (ht ('a 1) ('b 2) ('c 3))
                             ((ht a 'b ('c c)) (list a b c))))

    (check equal? '(1 (2 3)) (match (ht ('a 1) ('b 2) ('c 3))
                               ((ht 'a ((? symbol?) v) ...) (list a v))))

    (check equal? '(1) (match (ht ('a 1) ('b 2) ('c 3))
                         ((ht 'a _ ...) (list a))))

    ;; alists
    (check equal? '(1 2 3) (match (dict->list (ht ('a 1) ('b 2) ('c 3) ('d 4)))
                             ((ht a 'b ('c c)) (list a b c))))


    (check equal? '(1 (2 3)) (match (dict->list (ht ('a 1) ('b 2) ('c 3)))
                               ((ht 'a ((? symbol?) v) ...) (list a v))))

    (check equal? '(1) (match (dict->list (ht ('a 1) ('b 2) ('c 3)))
                         ((ht 'a _ ...) (list a))))))


(define (immutable-or-alist? t)
  (or (immutable? t) (alist? t)))


;; permissive ht* pattern
(define-match-expander ht*
  (syntax-parser

    ;; allow final repeating pattern: (keypat valpat) ...
    ((_ ht:ht ... (~seq htlast:ht (~literal ...)))
     #:with ht...    (if (attribute htlast) #'(htlast (... ...)) #'())
     #:with alist... (if (attribute htlast)
                         (if (attribute htlast.ignore)
                             #'(htlast (... ...))
                             #'((cons htlast.key htlast.var) (... ...)))
                         #'())

     ;; NOTE idea: => (app λ (list var ...) (ht-pat with final repeat pat only))
     ;; Where λ basically removes every key that appears as pattern from the
     ;; table, so that we can match the final repeat pattern against that trimmed
     ;; table. Ugly and expensive but it works.
     #`(app
        ;; transformer
        (λ (t)
          (define-values (t* remove)
            (if (immutable-or-alist? t)
                (values t dict-remove)
                (values (dict-copy t) (λ (t key) (dict-remove! t key) t))))
          ;; NOTE with mutable hash-tables the set! step is unnecessary so we
          ;; could just write (remove t* ht.key) ... and for that case it'd make
          ;; computation cheaper, but I don't want more branching here - this code
          ;; is painful enough.
          (set! t* (remove t* ht.key))
          ...
          (values (list (get: t ht.key) ...) t*))
        ;; patterns
        (list ht.var ...)
        (or (hash-table #,@#'ht...)
            (list-no-order #,@#'alist...))))

    ;; NOTE idea: => (app λ (list var ...)) where λ simply looks up every key in
    ;; the table with get:, accumulates results in a list to be matched
    ((_ ht:ht ...)
     #`(app (λ (t) (list (get: t ht.key) ...)) (list ht.var ...)))))


(module+ test

  (test-case "match with ht*-pattern"

    ;; hash-tables
    (check equal? (list 1 2 undefined) (match (ht ('a 1) ('b 2))
                                         ((ht* a b c) (list a b c))))

    (check equal? (list 1 2 undefined (set 4 5))
           (match (ht ('a 1) ('b 2) ('d 4) ('e 5))
             ((ht* a b c ((? symbol?) v) ...) (list a b c (list->set v)))))

    (check equal? (list 1 2 undefined (set '(e . 5) '(d . 4)))
           (match (ht ('a 1) ('b 2) ('d 4) ('e 5))
             ((ht* a b c (k v) ...) (list a b c (list->set (map cons k v))))))

    (check equal? (list 1 2 undefined)
           (match (ht ('a 1) ('b 2) ('d 4) ('e 5))
             ((ht* a b c _ ...) (list a b c))))

    ;; alists
    (check equal? (list 1 2 undefined) (match (dict->list (ht ('a 1) ('b 2)))
                                         ((ht* a b c) (list a b c))))

    (check equal? (list 1 2 undefined (set 4 5))
           (match (dict->list (ht ('a 1) ('b 2) ('d 4) ('e 5)))
             ((ht* a b c ((? symbol?) v) ...) (list a b c (list->set v)))))

    (check equal? (list 1 2 undefined (set '(e . 5) '(d . 4)))
           (match (dict->list (ht ('a 1) ('b 2) ('d 4) ('e 5)))
             ((ht* a b c (k v) ...) (list a b c (list->set (map cons k v))))))

    (check equal? (list 1 2 undefined)
           (match (dict->list (ht ('a 1) ('b 2) ('d 4) ('e 5)))
             ((ht* a b c _ ...) (list a b c))))))


;; TODO easy to implement standard ~> and ~>> in terms of my ~> below, not sure I
;; really need them, though. Only decent syntactic solution I can think of is to
;; have three macros: ~ ~> ~>>, but then how do I tell ~ as a hole from everything
;; else? Use _ instead maybe?

;; TODO it would be nice to break computation when #:guard fails, but what
;; semantics should it employ: essentially it needs to escape the entire ~> thread
;; and possibly return some value, defaulting to #f maybe? This means we need to
;; pass escape continuation, I guess e.g. to be invoked inside with (<~ val)?
;; Should we check for #:guard and #:do clauses first to see if continuation might
;; be used or let/ec doesn't make ~> more expensive?

(define-syntax (~> stx)

  (define (unbound? stx)
    (define top-level-or-unbound (not (identifier-binding stx)))
    (define not-top-level-bound (not (identifier-binding stx (syntax-local-phase-level) #t)))
    (and top-level-or-unbound
         not-top-level-bound))

  (define (~id? stx)
    (regexp-match #px"^~" (symbol->string (syntax-e stx))))

  (define-syntax-class ~
    (pattern id:id #:when (and (~id? #'id) (unbound? #'id))))

  (define-syntax-class clause
    #:attributes ((pre 1) hole (post 1))
    (pattern (pre ... hole:~ post ...))
    (pattern (pre ...)
             #:with (post ...) #'()
             #:attr hole #f))

  (define kw-table
    (list (list '#:guard check-expression)
          (list '#:do check-expression)
          (list '#:with check-identifier check-expression)))

  (define (fix-outer/ctx ctx stx [loc #f])
    (datum->syntax ctx (syntax-e stx) loc))

  (define (options->syntaxes options)
    (for/list ((opt (in-list options)))
      (match opt
        ((list #:with ctx id e)
         (with-syntax ((id id) (e e))
           (fix-outer/ctx ctx #'(define id e) ctx)))

        ((list #:do ctx body)
         (define/syntax-parse (e:expr ...) body)
         (fix-outer/ctx ctx #'(begin e ...) ctx))

        ((list-rest kw ctx _)
         (raise-syntax-error #f (format "unexpected keyword ~a" kw) ctx ctx)))))

  ;; TODO #:guard once I figure its semantics

  (syntax-parse stx

    ;; no more clauses
    ((_ e:expr) #'e)

    ;; keyword options before the next clause
    ((_ e:expr (~peek _:keyword) . rest)
     #:do ((define-values (options clauses)
             (parse-keyword-options #'rest kw-table
                                    #:context this-syntax)))
     #:with (clause ...) clauses
     #:with body (fix-outer/ctx this-syntax #'(~> val clause ...) this-syntax)
     #:with (options ...) (datum->syntax this-syntax (options->syntaxes options) this-syntax)
     (fix-outer/ctx this-syntax
                    #'(begin (define val e) options ... body)
                    this-syntax))

    ;; clause with hole
    ((_ e:expr c:clause rest ...)
     #:when (attribute c.hole)
     #:with clause/e (fix-outer/ctx this-syntax #'(c.pre ... e c.post ...) #'c)
     (fix-outer/ctx this-syntax #'(~> clause/e rest ...) this-syntax))

    ;; cluase with no hole
    ((_ e:expr c:clause rest ...)
     #:when (not (attribute c.hole))
     #:with clause (fix-outer/ctx this-syntax #'(c.pre ... c.post ...) #'c)
     (fix-outer/ctx this-syntax
                    #'(begin e (~> clause rest ...))
                    this-syntax))))


(module+ test

  (check-eq? (~> 'foo
                 (symbol->string ~)
                 (format ":~a" ~str)
                 (string->symbol ~))
             ':foo)

  (check-eq? (~> 'foo
                 (symbol->string ~)
                 (format ":~a" ~str)
                 ;; threading can be split by expr that ignores the result
                 (list 42)
                 (car ~))
             42)

  ;; exn: symbol->string: contract violation
  (check-exn exn:fail:contract?
             (thunk
              (~> '42
                  (symbol->string ~)
                  (format ":~a" ~str)
                  (string->symbol ~))))

  ;; ensure macro introduced val doesn't capture outside val
  (check-eq? (let ((val 0))
               (~> val
                   (+ 1 ~)
                   #:do ()
                   ;; val must still be 0
                   (+ val ~)))
             1)

  (check-equal? (~> 'foo
                    (symbol->string ~)
                    #:with bar "-bar"
                    #:with baz "-baz"
                    (string-append ~foo bar baz)
                    (format ":~a" ~str)
                    (string->symbol ~)
                    #:do ((define l (list 1 2))
                          (set! l (cons 0 l)))
                    (cons ~sym l))
                '(:foo-bar-baz 0 1 2)))
