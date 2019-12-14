#lang racket

(require digimon/number)
(require digimon/format)

(define cout
  (lambda [str n prefix]
    (when (integer? n)
      (define bits (integer-length n))
      (define hex (string-upcase (number->string n 16)))
      (define-values (size mod) (quotient/remainder (string-length hex) 2))
      (cond [(= mod 0) (printf "\"~a\", ~a, ~a, \"~a~a\"~n" hex size (integer-length n) prefix str)]
            [else (printf "\"0~a\", ~a, ~a, \"~a~a\"~n" hex (add1 size) (integer-length n) prefix str)]))))

(define arithmetic
  (lambda [op str-lhs str-rhs]
    (define lhs (string->number str-lhs 16))
    (define rhs (string->number str-rhs 16))
    (when (and (integer? lhs) (integer? rhs))
      (define n (op lhs rhs))
      (define hex (string-upcase (number->string n 16)))
      (define-values (size mod) (quotient/remainder (string-length hex) 2))
      (define expr (format "(~a #x~a #x~a)" (object-name op) str-lhs str-rhs))
      (cond [(= mod 0) (printf "\"~a\", ~a ~a~n" hex (integer-length n) expr)]
            [else (printf "\"0~a\", ~a ~a~n" hex (integer-length n) expr)]))))

(define bitwise
  (case-lambda
    [(op str-lhs)
     (define lhs (string->number str-lhs 16))
     (when (and (integer? lhs))
       (define n (op lhs))
       (define hex (bytes->hex-string (integer->network-bytes n)))
       (define-values (size mod) (quotient/remainder (string-length hex) 2))
       (define expr (format "(~a #x~a)" (object-name op) str-lhs))
       (cond [(= mod 0) (printf "\"~a\" ~a~n" hex expr)]
             [else (printf "\"0~a\" ~a~n" hex expr)]))]
    [(op str-lhs rhs)
     (define lhs (string->number str-lhs 16))
     (when (and (integer? lhs) (integer? rhs))
       (define n (op lhs rhs))
       (define hex (string-upcase (number->string n 16)))
       (define-values (size mod) (quotient/remainder (string-length hex) 2))
       (define expr (format "(~a #x~a ~a)" (object-name op) str-lhs rhs))
       (cond [(= mod 0) (printf "\"~a\" ~a~n" hex expr)]
             [else (printf "\"0~a\" ~a~n" hex expr)]))]
    [(op str-lhs start end)
     (define lhs (string->number str-lhs 16))
     (when (and (integer? lhs) (integer? start) (integer? end))
       (define n (op lhs start end))
       (define hex (string-upcase (number->string n 16)))
       (define-values (size mod) (quotient/remainder (string-length hex) 2))
       (define expr (format "(~a #x~a ~a ~a)" (object-name op) str-lhs start end))
       (cond [(= mod 0) (printf "\"~a\" ~a~n" hex expr)]
             [else (printf "\"0~a\" ~a~n" hex expr)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (cout str dec "#o")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'memory
(memory "10")
(memory "10121")
(memory "12345")
(memory "\0A\0B\0C\0D\0E\0F")

'hexadecimal
(hexadecimal "1234567890ABCDEF")
(hexadecimal "FEDCBA098765432")
(hexadecimal "000000789FEDCBA")

'decimal
(decimal "0000000890")
(decimal "123456789")
(decimal "098765432")

'octal
(octal "00000567")
(octal "0123456776543210")
(octal "7654321001234567")

'+
(arithmetic + "1" "0")
(arithmetic + "2" "113198824")
(arithmetic + "3" "0")
(arithmetic + "FF" "FFFF01")
(arithmetic + "2718281828459045" "3141592653589793")
(arithmetic + "6243299885435508" "6601618158468695")
(arithmetic + "7642236535892206" "9159655941772190")
(arithmetic + "161803398874989484820" "35323")
(arithmetic + "765432100123456789ABCDEF" "765432100123456789ABCDEF")
(arithmetic + "00000000765432100123456789ABCDEF" "FECDBA98765432100123456789ABCDEF")
(arithmetic + "FECDBA98765432100123456789ABCDEF" "FECDBA98765432100123456789ABCDEF")
(arithmetic + "3006050FB7A76AC18302FB593358" "20539")

'*
(arithmetic * "1" "0")
(arithmetic * "2" "113198824")
(arithmetic * "3" "0")
(arithmetic * "392" "1")
(arithmetic * "392" "54")
(arithmetic * "2718281828459045" "3141592653589793")
(arithmetic * "6243299885435508" "6601618158468695")
(arithmetic * "7642236535892206" "9159655941772190")
(arithmetic * "161803398874989484820" "1")
(arithmetic * "765432100123456789ABCDEF" "765432100123456789ABCDEF")
(arithmetic * "00000000765432100123456789ABCDEF" "FECDBA98765432100123456789ABCDEF")
(arithmetic * "FECDBA98765432100123456789ABCDEF" "FECDBA98765432100123456789ABCDEF")
(arithmetic * "3006050FB7A76AC18302FB593358" "20539")

'bitwise
(bitwise arithmetic-shift "2718281828459045" 10)
(bitwise arithmetic-shift "6243299885435508" 92)

(bitwise arithmetic-shift "2718281828459045" -10)
(bitwise arithmetic-shift "6243299885435508" -45)
(bitwise arithmetic-shift "FECDBA98765432100123456789ABCDEF" -92)
(bitwise arithmetic-shift "765432100123456789ABCDEF" -108)

(bitwise bitwise-and "ABC" 0)
(bitwise bitwise-and "ABCDEF" #xAB00FFFF00)
(bitwise bitwise-and "90ABCDEF" #xCD00FFFF00)
(bitwise bitwise-and "567890ABCDEF" #xEF00FFFF00)

(bitwise bitwise-ior "ABC" 0)
(bitwise bitwise-ior "ABCDEF" #xAB00FFFF00)
(bitwise bitwise-ior "90ABCDEF" #xCD00FFFF00)
(bitwise bitwise-ior "567890ABCDEF" #xEF00FFFF00)

(bitwise bitwise-xor "ABC" 0)
(bitwise bitwise-xor "ABC" #xABC)
(bitwise bitwise-xor "ABC" #xACD)
(bitwise bitwise-xor "ABCDEF" #xAB00FFCDEF)
(bitwise bitwise-xor "34567890ABCDEF" #xEF00FFFF00)
