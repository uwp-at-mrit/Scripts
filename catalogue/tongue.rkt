#lang racket

(provide (all-defined-out))

(struct tongue (id index en-US zh-CN) #:transparent)

(define namespace 'WarGrey::SCADA)

(define make-tongue-class
  (lambda [classname tongue data select #:/dev/stdout [/dev/stdout (current-output-port)]]
    (define ns::Tongue (format "~a::Tongue" namespace))
    (define ns::class (format "~a::~a" namespace classname))
    (define Tongue<E> (format "~a<~a>" ns::Tongue ns::class))
    
    (fprintf /dev/stdout "#pragma once~n")
    
    (fprintf /dev/stdout "~n#include ~s~n" "tongue.hpp")
    
    (fprintf /dev/stdout "~nnamespace ~a {~n" namespace)
    (fprintf /dev/stdout "    private class ~a : public ~a {~n" classname Tongue<E>)
    (fprintf /dev/stdout "        friend class ~a;~n" Tongue<E>)
    (fprintf /dev/stdout "    public:~n")
    (fprintf /dev/stdout "        static Platform::String^ type() { return ~s; }~n" tongue)

    (newline /dev/stdout)
    (fprintf /dev/stdout "    public:~n")
    (for ([datum (in-list data)])
      (fprintf /dev/stdout "        static ~a* ~a() { return ~a::UnsafeTongue(~aU); } // ~a~n"
               ns::class (tongue-id datum) Tongue<E> (tongue-index datum) (select datum)))

    (newline /dev/stdout)
    (fprintf /dev/stdout "    private:~n")
    (fprintf /dev/stdout "        static size_t indices_size() { return ~aU; }~n" (length data))
    (fprintf /dev/stdout "        static const unsigned int* indices() {~n")
    (fprintf /dev/stdout "            static const unsigned int indexes[] = {")
    (for ([datum (in-list (sort data < #:key tongue-index))]
          [index (in-naturals)])
      (cond [(zero? (remainder index 8)) (fprintf /dev/stdout "~a~n                ~aU" (if (zero? index) "" ",") (tongue-index datum))]
            [else (fprintf /dev/stdout ", ~aU" (tongue-index datum))]))
    (newline /dev/stdout)
    (fprintf /dev/stdout "            };~n")
    (newline /dev/stdout)
    (fprintf /dev/stdout "            return indexes;~n")
    (fprintf /dev/stdout "        }~n")

    (newline /dev/stdout)
    (fprintf /dev/stdout "    private:~n")
    (fprintf /dev/stdout "        ~a(unsigned int idx) : Tongue(idx) {}~n" classname)
    (fprintf /dev/stdout "    };~n")
    (fprintf /dev/stdout "}~n~n")))

(define make-tongue-resw
  (lambda [data select #:/dev/stdout [/dev/stdout (current-output-port)] #:default [default #false]]
    (fprintf /dev/stdout "<?xml version=~s encoding=~s ?>~n" "1.0" "UFT-8")
    (fprintf /dev/stdout "<root>~n")

    (unless (not default)
      (for ([datum (in-list data)])
      (fprintf /dev/stdout "    <data name=~s xml:space=~s> <value>~a</value> </data>~n"
               (number->string (tongue-index datum)) "preserve" (symbol->string (tongue-id datum)))))
    
    (for ([datum (in-list data)])
      (fprintf /dev/stdout "    <data name=~s xml:space=~s> <value>~a</value> </data>~n"
               (symbol->string (tongue-id datum)) "preserve" (select datum)))
    
    (fprintf /dev/stdout "</root>~n~n")))
