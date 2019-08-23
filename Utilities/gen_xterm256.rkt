#lang racket

(require bitmap/color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define println-index-syntax
  (lambda [idx]
    (define color (flcolor->hex (rgb* (xterma idx 1.0))))
    (printf "    this->xterms[~a] = Colours::make(0x~a);~n"
            idx (~r color #:base 16 #:min-width 6 #:pad-string "0"))))

(module+ main
  (for ([n (in-range 256)])
    (println-index-syntax n)))
