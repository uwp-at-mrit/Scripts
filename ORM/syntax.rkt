#lang racket

(provide (all-from-out db/base))
(provide (for-syntax (all-defined-out)))

(require db/base)

(require (for-syntax racket/list))
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(begin-for-syntax 
  (define (parse-field-definition tablename rowids stx)
    (syntax-parse stx
      [(field Type (~or (~optional (~or (~seq #:default defval) (~seq #:auto generate)) #:name "#:default or #:auto")
                        (~optional (~seq (~and #:not-null not-null)) #:name "#:not-null")
                        (~optional (~seq (~and #:unique unique)) #:name "#:unique")
                        (~optional (~seq #:% comments) #:name "#:%")) ...)
       (define-values (primary? not-null?) (values (and (member (syntax-e #'field) rowids) #true) (and (attribute not-null) #true)))
	   (values (and primary? #'Type)
               (list (if (or primary? not-null?) #'Type (format-id #'Type "std::optional<~a>" (syntax-e #'Type)))
                     (cond [(attribute defval) #'defval]
                           [(attribute generate) #'generate]
                           [else #'#false])
                     (or (attribute generate) #'#false)
                     (and not-null? #'#true)
                     (and (attribute unique) #'#true)))]))

  (define (parse-table-name stx)
    (syntax-parse stx
      [r:id (list #'r #'r)]
      [(r db) (list #'r #'db)]))
  
  (define (parse-primary-key stx)
    ; NOTE: primary keys may not contained in the defining struct in which case the struct is treated as a temporary view
    (syntax-parse stx
      [id:id (list #'id)]
      [(id0 id ...) stx]))

  (define (parse-order-by stx fields)
    (define (order-id <order> fields)
      (define order (syntax-e <order>))
      (cond [(memq order fields) order]
            [else (raise-syntax-error 'parse-order-by "#:order-by column is not defined" <order>)]))
    (syntax-parse stx
      [id:id (datum->syntax stx (symbol->string (order-id #'id fields)))]
      [sexp #'#false])))
