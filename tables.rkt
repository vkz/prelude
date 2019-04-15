#lang prelude


(require racket/struct
         racket/generic)


(module+ test
  (require rackunit))


;; TODO we must hide all lua struct fields, getters and setters - anything that
;; may hint to the user of prelude that tables aren't just values but structs.
;; IIUC struct and provide have necessary facilities, just need to use em.
(struct lua (table meta-table) #:mutable

  #:property prop:procedure dict-ref
  #:property prop:object-name (λ (self) (dict-ref self '__id))

  #:methods gen:dict

  ((define (dict-ref dict key [default (λ () undefined)])
     (if (hash-has-key? (lua-table dict) key)
         (hash-ref (lua-table dict) key)
         (if (lua-meta-table dict)
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


(define (rawset! t k v)
  (hash-set! (lua-table t) k v))


(module+ test

  (define t (table ('a 1) ('b 2)))

  (check eq? 1 (t 'a))
  (check eq? 2 (t 'b))
  (check-pred undefined? (t 'c))

  (dict-set! t 'c 42)
  (check eq? 42 (t 'c))

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

  (define proxy-table (table))
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

;;* Notes -------------------------------------------------------- *;;


;; TODO Milestone 1: all examples in Lua book Ch20 and Ch21 must work correctly.
;; TODO Milestone 2: #lang racket/tables should expose fancy {} syntax
;; TODO Milestone 3: FastCGI in #lang racket/tables


;; TODO {} constructor syntax e.g. {(key1 val1) (key2 val2)}. Maybe also support
;; extended "class" syntax {Class #:kw1 opt1 #:kw2 opt2 (k1 v1) (k2 v2)} where
;; Class is the meta-table to use and keyword options control behavior say
;; table immutability, concurrency behavior etc. We could also control
;;
;; simple constructor
;;   {('a 1)
;;    ('b 2)}
;;
;; extended constructor
;;   {foo #:kw1 option
;;        #:kw2 option
;;        ('a 1)
;;        ('b 2)}


;; TODO Define patterns for 'symbol keys:
;;   (define table.foo 42)
;;   (set! table.foo 42)
;;   (set: table.foo 42)
;;   (define (table.fun arg) 42)
;;   (define (table:fun arg) (list self 42))
;;
;; For any other key type simply use (set: obj field value)


;; TODO Access patterns for 'symbol keys:
;;   obj.field   (obj.method arg ...)
;;   obj:field   (obj:method self arg ...)
;;
;; For any other key simply use (get: obj field)


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
