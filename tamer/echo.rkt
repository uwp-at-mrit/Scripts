#lang racket

(provide (all-defined-out))

(require racket/string)

(define vim-colors
  #hash(("black" . 0) ("darkgray" . 8) ("darkgrey" . 8) ("lightgray" . 7) ("lightgrey" . 7) ("gray" . 7) ("grey" . 7) ("white" . 15)
                      ("darkred" . 1) ("darkgreen" . 2) ("darkyellow" . 3) ("darkblue" . 4) ("brown" . 5) ("darkmagenta" . 5)
                      ("darkcyan" . 6) ("red" . 9) ("lightred" . 9) ("green" . 10) ("lightgreen" . 10) ("yellow" . 11) ("lightyellow" . 11)
                      ("blue" . 12) ("lightblue" . 12) ("magenta" . 13) ("lightmagenta" . 13) ("cyan" . 14) ("lightcyan" . 14)))

(define term-colorize
  (lambda [fg bg attrs content]
    (define color-code
      (lambda [color #:bgcolor? [bg? #false]]
        (format "~a8;5;~a" (if bg? 4 3) (if (regexp-match? #px"\\d+" color) color (hash-ref vim-colors color)))))
    (regexp-replace #px"^(\\s*)(.+?)(\\s*)$" content
                    (format "\\1\033[~a;~a;~am\\2\033[0m\\3"
                            (string-replace (for/fold ([effects ""]) ([attr (in-list attrs)])
                                              (case (string-downcase (format "~a" attr))
                                                [{"bold" "bright"} (string-append effects ";1")]
                                                [{"dim"} (string-append effects ";2")]
                                                [{"underline" "undercurl"} (string-append effects ";4")]
                                                [{"blink"} (string-append effects ";5")]
                                                [{"reverse" "inverse"} (string-append effects ";7")]
                                                [{"hidden" "password"} (string-append effects ";8")]
                                                [else (error 'tarminal-colorize "Unsupported Terminal Attribute: ~a" attr)]))
                                            "^;" "" #:all? #false)
                            (if (not fg) 39 (color-code (string-downcase (format "~a" fg))))
                            (if (not bg) 49 (color-code (string-downcase (format "~a" bg)) #:bgcolor? #true))))))

(define echof
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))

(define eechof
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))
