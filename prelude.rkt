#lang racket

(require racket/generic)

(define-syntax-rule (comment . any) (void))
(define-syntax-rule (example . any) (void))

(define v (vector 'a 'b))
(define h (hash 'a 1 'b v))

(comment
 (let* ((table1 h)
        (table2 (dict-ref table1 'b)))
   (if (immutable? table2) (set! table2 (dict-set table2 0 42)) (dict-set! table2 0 42))
   (if (immutable? table1) (set! table1 (dict-set table1 'b table2)) (dict-set! table1 'b table1))
   table1)
 ;; comment
 )

(define-generics associative
  (assoc-get associative key)
  (assoc-set associative key val)
  #:fast-defaults ((dict?
                    (define (assoc-get table key) (dict-ref table key))
                    (define (assoc-set table key val)
                      (if (immutable? table)
                          (dict-set table key val)
                          (begin
                            (dict-set! table key val)
                            table))))
                   (list?
                    (define (assoc-get table key)
                      (unless (integer? key)
                        (error "Expected integer key"))
                      (list-ref table key))
                    (define (assoc-set table key val)
                      (unless (integer? key)
                        (error "Expected integer key"))
                      (list-set table key val)))))


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
     (assoc-set table key (apply set: (assoc-get table key) next-key more)))))


;; TODO #:failure thunk
(get: h 'b 1)
;; TODO on missing key insert a fresh dictionary, control the type of dict with
;; parameter
(set: h 'b 0 42)

(require racket/undefined)
(dict-ref h 'c (thunk undefined))
