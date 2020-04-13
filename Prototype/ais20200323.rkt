#lang racket

(provide ais-station)

(require racket/tcp)
(require racket/date)

(date-display-format 'iso-8601)

; YES, the message format is the same as NMEA 0183
(define nmeas (file->lines (path-replace-extension (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)) #".ais")))

(define (ais-station [interval 2.0] [ais-port 4003])
  (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "AIS: ~a~n" (exn-message e)))])
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let ([listener (tcp-listen ais-port 256 #true)])
                (define-values (hostname port _r _p) (tcp-addresses listener #true))
                (printf "> AIS:~a:~a [~a]~n" hostname port (date->string (current-date) #true))
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
                    (sleep interval))
                  (loop))))
       (thunk (custodian-shutdown-all (current-custodian))))))
  (sleep 1)
  (ais-station))

(module+ main
  (with-handlers ([exn? void])
    (ais-station)))
