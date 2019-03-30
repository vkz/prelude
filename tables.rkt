#lang prelude


(require racket/struct
         racket/generic)


(module+ test
  (require rackunit))


(struct lua (table meta-table) #:mutable

  #:property prop:procedure dict-ref

  #:methods gen:dict

  ((define (dict-ref dict key [default (λ () undefined)])
     (if (hash-has-key? (lua-table dict) key)
         (hash-ref (lua-table dict) key)
         (if (lua-meta-table dict)
             (let ((index (dict-ref (lua-meta-table dict) '__index)))
               (if (lua? index)
                   (index key)
                   (index dict key)))
             undefined)))

   (define/generic super-dict-set! dict-set!)
   (define (dict-set! dict key v)
     (super-dict-set! (lua-table dict) key v))

   (define/generic super-dict-remove! dict-remove!)
   (define (dict-remove! dict key)
     (super-dict-remove! (lua-table dict) key))

   (define/generic super-dict-iterate-first dict-iterate-first)
   (define (dict-iterate-first dict)
     (super-dict-iterate-first (lua-table dict)))

   (define/generic super-dict-iterate-next dict-iterate-next)
   (define (dict-iterate-next dict pos)
     (super-dict-iterate-next (lua-table dict) pos))

   (define/generic super-dict-iterate-key dict-iterate-key)
   (define (dict-iterate-key dict pos)
     (super-dict-iterate-key (lua-table dict) pos))

   (define/generic super-dict-iterate-value dict-iterate-value)
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



(define-syntax-rule (table arg ...)
  (lua (ht arg ...) undefined))


(define (set-meta-table! table meta-table)
  (unless (hash-has-key? meta-table '__index)
    (hash-set! meta-table '__index (table)))
  (set-lua-meta-table! table meta-table)
  table)


(module+ test

  (define t (table ('a 1) ('b 2)))

  (check eq? 1 (t 'a))
  (check eq? 2 (t 'b))
  (check eq? undefined (t 'c))

  (set-lua-meta-table! t (table ('__index (table ('c 3)))))
  (check eq? 3 (t 'c))

  (set-lua-meta-table! t (table ('__index (λ (self key) 42))))
  (check eq? 1 (t 'a))
  (check eq? 42 (t 'c)))
