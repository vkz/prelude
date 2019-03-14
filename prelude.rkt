#lang racket

(require syntax/parse (for-syntax syntax/parse))

(define-syntax-rule (comment . any) (void))
(define-syntax-rule (example . any) (void))


(require racket/generic)
(require racket/undefined)


(module+ test
  (require rackunit)

  (define v (vector 'a 'b))
  (define h (hash 'a 1 'b v))
  (define h! (make-hash `((a . 1) (b . ,v)))))


(define associative-on-key-missing (make-parameter make-hash))


(define-generics associative
  (assoc-get associative key [default])
  (assoc-set associative key val)
  #:fast-defaults ((dict?
                    ;; TODO should I return undefined on key missing instead of
                    ;; throwing like dict does?
                    (define (assoc-get table key [default (lambda () (error "key not found" key))])
                      (dict-ref table key default))
                    (define (assoc-set table key val)
                      (if (immutable? table)
                          (dict-set table key val)
                          (begin
                            (dict-set! table key val)
                            table))))
                   ;; TODO rethink this cause gen:dict is implemented for list? of
                   ;; pairs i.e. an alist, so I probably ought to follow suite. We
                   ;; typically want integer refs for vectors anyway
                   (list?
                    (define (assoc-get table key)
                      (unless (integer? key)
                        (error "Expected integer key"))
                      (list-ref table key))
                    (define (assoc-set table key val)
                      (unless (integer? key)
                        (error "Expected integer key"))
                      (list-set table key val)))))


;; TODO [#:default thunk] keyword arg that returns default value on key missing
(define (get: table key . keys)
  (unless (associative? table)
    (error "Expected associative"))
  (if (empty? keys)
      (assoc-get table key)
      (apply get: (assoc-get table key) keys)))


(define set:
  (case-lambda
    ((table key val) (assoc-set table key val))
    ((table key next-key . more)
     (assoc-set table key (apply set: (assoc-get table key (associative-on-key-missing)) next-key more)))))


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
  (check-pred immutable? (get: h 'c 'd)))


(comment
 ;; TODO #:associative struct prop
 (struct foo (v) #:associative)
 ;; =>
 (struct foo (v)
   #:methods gen:associative
   ((define (assoc-get))
    (define (assoc-set))))
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


;; TODO maybe #lang prelude with better defaults
