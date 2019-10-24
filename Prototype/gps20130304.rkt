#lang racket

(require racket/tcp)

(define nmeas (file->lines "gps20130304.txt"))

(define (server)
  (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let ([listener (tcp-listen 4006 256 #true)])
                (define-values (hostname port _r _p) (tcp-addresses listener #true))
                (printf "> ~a:~a~n" hostname port)
                (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener))

                (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                (printf "Greetings, ~a:~a.~n" remote rport)

                (let loop ()
                  (for ([nmea (in-list nmeas)])
                    (printf "[~a bytes] ~a~n" (+ (string-length nmea) 2) nmea)
                    (fprintf /dev/tcpout nmea)
                    (write-char #\return /dev/tcpout)
                    (write-char #\linefeed /dev/tcpout)
                    (flush-output /dev/tcpout)
                    (sleep 0.1))
                  (loop))))
       (thunk (custodian-shutdown-all (current-custodian))))))
  (sleep 1)
  (server))

(with-handlers ([exn? void])
  (server))
