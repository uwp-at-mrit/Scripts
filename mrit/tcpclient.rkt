#lang racket

(require "message.rkt")

(require racket/tcp)
(require file/sha1)

(with-handlers ([exn:break? void])
  (let connect-send-wait-loop ()
    (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
      (parameterize ([current-custodian (make-custodian)])
        (dynamic-wind
         (thunk (void))
         (thunk (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break "192.168.8.100" 2100)])
                  (write-mrmsg /dev/tcpout 65 98 1 4571)
                  (define-values (signature data) (read-mrmsg /dev/tcpin))
                  (displayln signature)))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))
