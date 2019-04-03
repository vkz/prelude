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
         ht hti
         kv)


(require syntax/parse
         (for-syntax syntax/parse)
         racket/generic
         racket/undefined)


(define-syntax-rule (comment . any) (void))
(define-syntax-rule (example . any) (void))


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


(define-syntax (ht stx)
  (syntax-parse stx
    ;; with equality
    ((_ #:eq op:expr (key:expr value:expr) ...)
     #'(parameterize ((ht-equality op)) ((ht-constructor) `((,key . ,value) ...))))

    ((_ (key:expr value:expr) ... #:eq op:expr)
     #'(parameterize ((ht-equality op)) ((ht-constructor) `((,key . ,value) ...))))

    ;; with default equality
    ((_ (key:expr value:expr) ...)
     #'((ht-constructor) `((,key . ,value) ...)))))


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
      (check-pred hash-eqv? h))))


(comment
 ;; NOTE basic idea for ht match-expander
 (match (dict->list (ht ('a 1) ('b 2)))
   ((hash-table ('b v)) v)
   ((list-no-order (cons 'b vb) (cons 'a va) _ ...) (list va vb)))
 ;; comment
 )


(begin-for-syntax
  (define-syntax-class ht
    (pattern ((~datum quote) var:id) #:with key this-syntax)
    (pattern (key var))
    (pattern var:id #:with key (datum->syntax #'var `',(syntax-e #'var)))))


;; TODO I'd much rather have this expander be called `ht', alas `ht' is already
;; bound to our table constructor macro. I wonder if there's a way to have both.
(define-match-expander kv
  (syntax-parser

    ;; allow final repeating pattern: (keypat valpat) ...
    ((_ ht:ht ... (~seq htlast:ht (~literal ...)))
     #:with ht...    (if (attribute htlast) #'(htlast (... ...)) #'())
     #:with alist... (if (attribute htlast) #'((cons htlast.key htlast.var) (... ...)) #'())
     ;; TODO to stay true to types that implement dict interface this matcher
     ;; should have another branch to somehow work for vectors but I wonder if
     ;; that'd be at all useful.
     #`(or (hash-table (ht.key ht.var) ... #,@#'ht...)
           (list-no-order (cons ht.key ht.var) ... #,@#'alist...)))

    ;; only key val patterns
    ((_ ht:ht ...)
     #`(or (hash-table (ht.key ht.var) ...)
           (list-no-order (cons ht.key ht.var) ... _ (... ...))))))


(module+ test

  (check equal? '(1 2 3) (match (ht ('a 1) ('b 2) ('c 3))
                           ((kv a 'b ('c c)) (list a b c))))

  (check equal? '(1 2 3) (match (dict->list (ht ('a 1) ('b 2) ('c 3) ('d 4)))
                           ((kv a 'b ('c c)) (list a b c))))

  ;; final pat ... should work for hash-tables
  (check equal? '(1 (2 3)) (match (ht ('a 1) ('b 2) ('c 3))
                             ((kv 'a ((? symbol?) v) ...) (list a v))))

  ;; ditto for alists
  (check equal? '(1 (2 3)) (match (dict->list (ht ('a 1) ('b 2) ('c 3)))
                             ((kv 'a ((? symbol?) v) ...) (list a v)))))


;; TODO maybe #lang prelude with better defaults
