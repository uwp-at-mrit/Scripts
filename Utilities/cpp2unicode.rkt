#lang racket/gui

(define gbk->utf8
  (lambda [src.cpp]
    (define /dev/gbkin (reencode-input-port (open-input-file src.cpp) "GB18030" #false #true))
    (define cpp (port->string /dev/gbkin))
    (close-input-port /dev/gbkin)
    (call-with-output-file* src.cpp #:exists 'truncate/replace
      (λ [/dev/utf8out] (displayln cpp /dev/utf8out)))
    (printf "[Converted]~a~n" src.cpp)))

(define cpp-root (get-directory "Choose the root directory of the cpp sources"))

(when (path? cpp-root)
  (for ([src.cpp (in-directory cpp-root)] #:when (regexp-match? #px"[.]cpp$" src.cpp))
    (define maybe-ok (with-handlers ([exn? values]) (bytes->string/utf-8 (file->bytes src.cpp))))
    (cond [(not (exn? maybe-ok)) (printf "[Skipped]~a~n" src.cpp)]
          [else (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "[Failed]~a: ~a~n" (exn-message e)))])
                  (gbk->utf8 src.cpp))]))

  (for ([src.h (in-directory cpp-root)] #:when (regexp-match? #px"[.]h$" src.h))
    (define maybe-ok (with-handlers ([exn? values]) (bytes->string/utf-8 (file->bytes src.h))))
    (cond [(not (exn? maybe-ok)) (printf "[Skipped]~a~n" src.h)]
          [else (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "[Failed]~a: ~a~n" (exn-message e)))])
                  (gbk->utf8 src.h))])))
