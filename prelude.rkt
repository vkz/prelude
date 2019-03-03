#lang racket


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
