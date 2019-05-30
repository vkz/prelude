#lang prelude


(require racket/struct
         racket/generic
         (for-syntax syntax/parse
                     racket/match))

(module+ test
  (require rackunit)
  (require prelude/testing))


;;* Table struct ------------------------------------------------- *;;


;; NOTE Replace apply with keyword-apply if some dict method takes kw args
(define ((redirect-generic generic) t . args)
  (apply generic (table-dict t) args))


(struct table (dict meta) #:mutable

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

  (test-case "gen:dict"
    (define t (table (ht ('a 1)) undefined))
    (check-true (dict-has-key? t 'a))
    (check-eq? (dict-ref t 'a) 1)
    (check-true (undefined? (dict-ref t 'b)))
    (check-not-exn (thunk (dict-set! t 'b 2)))
    (check-eq? (dict-ref t 'b) 2))

  (test-case "gen:associative"
    (define tb (table (ht ('b 1)) undefined))
    (define ta (table (ht ('a tb)) undefined))
    (check-eq? (get: ta 'a 'b) 1)
    (check-not-exn (thunk (set: ta 'a 'c 2)))
    (check-eq? (get: ta 'a 'c) 2)))


;;* <get> metamethod --------------------------------------------- *;;


;; TODO consider optional default proc argument?
(define (get t key)
  (if (dict-has-key? t key)
      (dict-ref t key)
      (if (table? (table-meta t))
          (let* ((mt (table-meta t))
                 (metamethod (dict-ref mt ':<get>)))
            (if metamethod
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
                     (metamethod (dict-ref mt ':<insert>)))
                (cond
                  ((table? metamethod) (set metamethod key v))
                  ((procedure? metamethod) (metamethod t key v))
                  ;; TODO should we signal an error?
                  (else (dict-set! t key v))))
              (dict-set! t key v))))
  t)


(module+ test

  (define/checked mt (table (ht ('b 2)) undefined))
  (define/checked t (table #;t (ht ('a 1)) #;mt mt))
  (define/checked tt (table #;t (ht) #;mt t))
  (define/checked t<get>proc (table #;t (ht) #;mt (table (ht (':<get> (λ (_ key) (get t key)))) undefined)))
  (define/checked t<get>table (table #;t (ht) #;mt (table (ht (':<get> t)) undefined)))

  (test-case "get: when mt is a table"
    (check-eq? (get t 'a) 1)
    (check-eq? (get t 'b) 2)
    (check-pred undefined? (get t 'c))
    ;; deeper mt chain
    (check-eq? (get tt 'a) 1)
    (check-eq? (get tt 'b) 2)
    (check-pred undefined? (get tt 'c)))

  (test-case "get: when <get> metamethod is a procedure"
    (check-eq? (get t<get>proc 'a) 1)
    (check-eq? (get t<get>proc 'b) 2)
    (check-pred undefined? (get t<get>proc 'c)))

  (test-case "get: when <get> metamethod is a table"
    (check-eq? (get t<get>table 'a) 1)
    (check-eq? (get t<get>table 'b) 2)
    (check-pred undefined? (get t<get>table 'c)))

  (test-case "set: with no <insert> metamethod"
    ;; insert
    (check-not-exn (thunk (set t 'c 3)))
    (check-eq? (get t 'c) 3)
    ;; update
    (check-not-exn (thunk (set t 'a 0)))
    (check-eq? (get t 'a) 0))

  (test-case "set: when <insert> metamethod is a table"
    (check-not-exn (thunk (set mt ':<insert> mt)))
    ;; insert
    (check-not-exn (thunk (set t 'd 4)))
    (check-eq? (get mt 'd) 4)
    (check-eq? (get t 'd) 4)
    ;; update inserted
    (check-not-exn (thunk (set t 'd 0)))
    (check-eq? (get mt 'd) 0)
    ;; update existing
    (check-not-exn (thunk (set t 'a -1)))
    (check-eq? (get t 'a) -1))

  (test-case "set: when <insert> metamethod is a procedure"
    (check-not-exn (thunk (set mt ':<insert> (λ (_ k v) (set mt k v)))))
    (check-not-exn (thunk (set t 'e 5)))
    (check-eq? (get mt 'e) 5)
    (check-eq? (get t 'e) 5)))
