#lang racket


(require (for-syntax syntax/parse)
         syntax/parse/define
         racket/undefined
         rackunit)


(provide checked
         define/checked
         (all-from-out rackunit))


(define (syntax->location stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))


(define (list/if . vs) (filter values vs))


(define-check (set/check! box v)
  (set-box! box v))


;; NOTE why `checked` and `define/checked`? Because rackunit checks return (void).
;; I fail to see the benefit of this decision.


(define-syntax (checked stx)
  (syntax-parse stx
    [(_ e:expr msg)
     (quasisyntax
      (begin
        (when msg
          (unless (string? msg)
            (raise-argument-error
             (string->symbol "msg") "string? or #f" msg)))
        (with-default-check-info*
          (list/if (make-check-name 'checked)
                   (and msg (make-check-info 'checked-message msg))
                   (make-check-location
                    (syntax->location #'e))
                   (make-check-expression 'e)
                   (make-check-params 'e))
          (thunk
           (let ((result (box undefined)))
             ;; NOTE the only reason we wrap e in check is to have rackunit
             ;; exception handlers installed. We don't wrap in any predefined
             ;; checks cause they gobble up errortraces. Rackunit needs to be
             ;; retired, darn.
             (set/check! result e)
             (unbox result))))))]
    [(_ e:expr)
     #'(checked e #f)]))


(define-simple-macro (define/checked name:id e:expr)
  (define name (checked e (format "failed to define ~a" 'name))))


(module+ test
  (require rackunit/private/check-info)

  (define (fail)
    (define foo 'bar)
    (error 'oops)
    42)

  ;; NOTE can't wrap this in test-case cause it somehow overrides parameters and
  ;; current-check-handler below never runs
  (let ((info #f))
    (parameterize ((current-check-handler
                    (λ (exn)
                      (set! info (current-check-info))
                      #;(pretty-print info))))
      (define/checked baz (+ 1 (fail)))
      (void))
    (check-eq? (check-info-value (check-info-ref info 'name)) 'checked)
    (check-true (check-info-contains-key? info 'checked-message))
    (check-true (check-info-contains-key? info 'location))
    (check-equal? (pretty-info-value (check-info-value (check-info-ref info 'params)))
                  '(+ 1 (fail))))

  (let ((info #f))
    (parameterize ((current-check-handler
                    (λ (exn)
                      (set! info (current-check-info))
                      #;(pretty-print info))))
      (checked (fail) "message override")
      (void))
    (check-equal? (check-info-value (check-info-ref info 'checked-message))
                  "message override"))

  (test-case  "define/checked succeeds"
    (let ((result #f))
      (check-not-exn (thunk (define/checked foo (checked 'foo)) (set! result foo)))
      (check-eq? 'foo result))))
