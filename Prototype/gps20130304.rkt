#lang racket

(provide gps-server)

(require racket/tcp)
(require racket/date)

(date-display-format 'iso-8601)

(define nmeas (file->lines (path-replace-extension (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)) #".gps")))

(define (gps-server [interval 0.1] [gps-port 4006])
  (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "GPS: ~a~n" (exn-message e)))])
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let ([listener (tcp-listen gps-port 256 #true)])
                (define-values (hostname port _r _p) (tcp-addresses listener #true))
                (printf "> GPS:~a:~a [~a]~n" hostname port (date->string (current-date) #true))
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
  (gps-server))

(module+ main
  (with-handlers ([exn? void])
    (gps-server)))
