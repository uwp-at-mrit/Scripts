#lang racket

(require "catalogue/tongue.rkt")

(define force-remake (make-parameter #false))

(define resource-exists?
  (lambda [tongue.resw.rkt]
    (regexp-match? #px"[.]resw[.]rkt$" tongue.resw.rkt)))

(define do-make-resw
  (lambda [cat tongue]
    (printf "> cat ~a~n" (file-name-from-path tongue))
    (parameterize ([current-output-port (open-output-nowhere)])
      (call-with-output-file* tongue #:exists 'truncate/replace cat))
    (call-with-input-file* tongue (curryr copy-port (current-output-port)))))

(define make-resws
  (lambda [tongue.resw.rkt tongue-root]
    (define tongue (cadr (regexp-match #px"(.+)[.][^.]+[.]rkt$" (file-name-from-path tongue.resw.rkt))))
    (define tongue.hpp (build-path (path-only tongue.resw.rkt) (path-add-extension tongue #".hpp")))
    (define tongue.en (build-path tongue-root "en-US" (path-add-extension tongue #".resw")))
    (define tongue.zh (build-path tongue-root "zh-CN" (path-add-extension tongue #".resw")))

    (define rkt.mtime (file-or-directory-modify-seconds tongue.resw.rkt))
    (define hpp.mtime (if (or (force-remake) (not (file-exists? tongue.hpp))) (- rkt.mtime 1) (file-or-directory-modify-seconds tongue.hpp)))
    (define en.mtime (if (or (force-remake) (not (file-exists? tongue.en))) (- rkt.mtime 1) (file-or-directory-modify-seconds tongue.en)))
    (define zh.mtime (if (or (force-remake) (not (file-exists? tongue.zh))) (- rkt.mtime 1) (file-or-directory-modify-seconds tongue.zh)))

    (when (or (> rkt.mtime hpp.mtime) (> rkt.mtime en.mtime) (> rkt.mtime zh.mtime))
      (define main (dynamic-require tongue.resw.rkt 'main void))
      (when (procedure? main)
        (define-values (classname data) (main))
        (define identities (make-hasheq))

        (for ([datum (in-list data)])
          (hash-update! identities (tongue-id datum) add1 (λ [] 0)))

        (define duplicates (filter (λ [id] (> (cdr id) 1)) (hash->list identities)))
        (if (pair? duplicates)
            (void (displayln "duplicate id:")
                  (for ([id (in-list duplicates)])
                    (printf "  '~a: ~a~n" (car id) (cdr id))))
            (void (when (> rkt.mtime hpp.mtime)
                    (do-make-resw (λ [/dev/stdout] (make-tongue-class classname tongue data tongue-en-US #:/dev/stdout /dev/stdout)) tongue.hpp))
        
                  (newline)
                  (newline)
                  (newline)
                  
                  (when (> rkt.mtime en.mtime)
                    (do-make-resw (λ [/dev/stdout] (make-tongue-resw data tongue-en-US #:default #true #:/dev/stdout /dev/stdout)) tongue.en))
                  
                  (newline)
                  (newline)
                  (newline)
                  
                  (when (> rkt.mtime zh.mtime)
                    (do-make-resw (λ [/dev/stdout] (make-tongue-resw data tongue-zh-CN #:default #false #:/dev/stdout /dev/stdout)) tongue.zh))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (command-line
   #:program "mkresw"
   #:once-each
   [("-f" "--force") "force remake string resources" (force-remake #true)]
   #:args [sln-root]
   (when (directory-exists? sln-root)
     (define all-projects (filter directory-exists? (directory-list sln-root #:build? #true)))
     (for ([project-root (in-list all-projects)])
       (define tongue-root (build-path project-root "stone" "tongue"))
       (when (directory-exists? tongue-root)
         (for ([resw (in-list (filter resource-exists? (directory-list tongue-root #:build? #true)))])
           (make-resws resw tongue-root)))))))
