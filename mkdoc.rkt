#lang racket

(require "Handbook/env.rkt")

(require compiler/cm)

(define &again? (box #false))
(define compiling-round 1)

(define compile-docs
  (lambda [handbook.scrbl [round 1]]
    (set-box! &again? #false)
    (define (filter-verbose info)
      (define px.in (pregexp (path->string (current-directory))))
      (define traceln (curry printf "round[~a]: ~a~n" round))
      (match info
        [(pregexp #px"checking:") '|Skip Checking|]
        [(pregexp #px"compiling ") (set-box! &again? #true)]
        [(pregexp #px"done:") (when (regexp-match? px.in info) (traceln info) (set-box! &again? #true))]
        [(pregexp #px"maybe-compile-zo starting") (traceln info)]
        [(pregexp #px"(wrote|compiled|processing:|maybe-compile-zo finished)") '|Skip Task Endline|]
        [(pregexp #px"(newer|skipping:)") '|Skip Reasons|]
        [_ (traceln info)]))
    (parameterize ([manager-trace-handler filter-verbose])
      (managed-compile-zo handbook.scrbl))

    (when (unbox &again?)
      (compile-docs handbook.scrbl (add1 round)))))

(define make-docs
  (lambda [handbook.scrbl]
    (compile-docs handbook.scrbl)
    (parameterize ([current-namespace (make-base-namespace)])
      (parameterize ([exit-handler (thunk* (error 'make "[fatal] ~a needs a proper `exit-handler`!" handbook.scrbl))])
        ;;; WARNING: the eval namespace does not share handbook module with the main namespace.
        (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
        (eval `(render (list ,(dynamic-require handbook.scrbl 'doc)) (list ,(file-name-from-path handbook.scrbl))
                       #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %)))
                       #:dest-dir ,(handbook-dest) #:xrefs (list (load-collections-xref))
                       #:quiet? #false #:warn-undefined? #false))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define mkdoc.rkt (simplify-path (build-path (find-system-path 'orig-dir) (find-system-path 'run-file))))

  (parameterize ([sln-root (find-solution-root-dir (path-only mkdoc.rkt))])
    (when (sln-root)
      (define all-projects (filter directory-exists? (directory-list (sln-root) #:build? #true)))
      (for ([project-root (in-list all-projects)])
        (define handbook.scrbl (handbook-entry project-root))
        (when (file-exists? handbook.scrbl)
          (parameterize ([current-directory project-root]
                         [current-custodian (make-custodian)])
            (define /dev/hbout (open-output-string))
            (with-handlers ([exn:fail? (λ [e] (displayln e))])
              (parameterize ([current-output-port /dev/hbout])
                (make-docs handbook.scrbl)))

            (define maybe-output-to (get-output-string /dev/hbout))
            (with-handlers ([exn:fail? (λ [e] (displayln maybe-output-to))])
              (define /dev/hbin (open-input-string maybe-output-to))
              (let read-with ([last-line #false])
                (define line (read-line /dev/hbin))
                (cond [(string? line) (when (string? last-line) (displayln last-line)) (read-with line)]
                      [else (let ([lsize (string-length last-line)]
                                  [prefix "Output to"])
                              (define index.html (substring last-line (+ (string-length prefix) 3) (- lsize 1)))
                              (system (format "open '~a'" index.html))
                              (printf "[~a ~a]~n" prefix index.html))])))
            (custodian-shutdown-all (current-custodian))))))))