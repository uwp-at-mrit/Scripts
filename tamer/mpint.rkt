#lang racket

(require digimon/number)

(define base256
  (lambda [str]
    (define n (network-bytes->natural (string->bytes/utf-8 str)))
    (define hex (number->string n 16))
    (define-values (size mod) (quotient/remainder (string-length hex) 2))
    (printf "\"~a\", ~a, ~a, \"#x~a\"~n" hex (if (= mod 0) size (add1 size)) (integer-length n) str)))

(define hexadecimal
  (lambda [str]
    (define n (string->number str 16))
    
    (when (integer? n)
      (define hex (number->string n 16))
      (define-values (size mod) (quotient/remainder (string-length hex) 2))
      (printf "\"~a\", ~a, ~a, \"#x~a\"~n" hex (if (= mod 0) size (add1 size)) (integer-length n) str))))

(define decimal
  (lambda [str]
    (define dec (string->number str 10))
    
    (when (integer? dec)
      (define hex (number->string dec 16))
      (define-values (size mod) (quotient/remainder (string-length hex) 2))
      (printf "\"~a\", ~a, ~a, \"~a\"~n" hex (if (= mod 0) size (add1 size)) (integer-length dec) str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(base256 "1234567890ABCDEF")

(hexadecimal "1234567890ABCDEF")
(hexadecimal "FEDCBA098765432")
(hexadecimal "000000789FEDCBA")

(decimal "890")
(decimal "1234567890")
(decimal "098765432")
(decimal "000000789")
