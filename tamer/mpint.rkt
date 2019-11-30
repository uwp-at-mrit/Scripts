#lang racket

(require digimon/number)

(define cout
  (lambda [str n prefix]
    (when (integer? n)
      (define bits (integer-length n))
      (define hex (string-upcase (number->string n 16)))
      (define-values (size mod) (quotient/remainder (string-length hex) 2))
      (cond [(= mod 0) (printf "\"~a\", ~a, ~a, \"~a~a\"~n" hex size (integer-length n) prefix str)]
            [else (printf "\"0~a\", ~a, ~a, \"~a~a\"~n" hex (add1 size) (integer-length n) prefix str)]))))

(define memory
  (lambda [str]
    (define n (network-bytes->natural (string->bytes/utf-8 str)))
    (cout str n "#F")))

(define hexadecimal
  (lambda [str]
    (define n (string->number str 16))
    (cout str n "#x")))

(define decimal
  (lambda [str]
    (define dec (string->number str 10))
    (cout str dec "")))

(define octal
  (lambda [str]
    (define dec (string->number str 8))
    (cout str dec "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(memory "1234567890ABCDEF")
(memory "\0A\0B\0C\0D\0E\0F")

(hexadecimal "1234567890ABCDEF")
(hexadecimal "FEDCBA098765432")
(hexadecimal "000000789FEDCBA")

(decimal "0000000890")
(decimal "123456789")
(decimal "098765432")

(octal "00000567")
(octal "0123456776543210")
(octal "7654321001234567")
