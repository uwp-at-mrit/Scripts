#lang racket/gui

(define src-root (get-directory "Choose the root directory of the sources"))

(define source-extensions '(cxx cpp hxx hpp rkt))

(define path-source-extension
  (lambda [src]
    (define maybe-ext (path-get-extension src))
    (and (bytes? maybe-ext)
         (let ([/dev/extin (open-input-bytes maybe-ext)])
           (read-char /dev/extin)
           (let ([ext (read /dev/extin)])
             (and (memq ext source-extensions)
                  ext))))))

(when (path? src-root)
  (for/fold ([locs (make-immutable-hasheq)]
             [total 0])
            ([src.cpp (in-directory src-root)])
    (define ext (path-source-extension src.cpp))
    (cond [(not ext) (values locs total)]
          [else (let ([loc (length (file->lines src.cpp))])
                  (printf "~a: ~a~n" src.cpp loc)
                  (values (hash-set locs ext (+ (hash-ref locs ext 0) loc))
                          (+ total loc)))])))
