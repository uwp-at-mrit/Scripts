#lang racket

(require racket/tcp)

(define nmeas
  (list "$GPGGA,092911.00,2228.52704635,N,11352.02447428,E,2,08,1.1,30.776,M,-2.801,M,5.0,0129*66"
        (list "$GPVTG,343.71,T,,M,5.88,N,10.88,K,D*0E"
              "$GPHDT,343.515,T*30")
        "$GPGGA,092911.20,2228.52736105,N,11352.02437292,E,2,08,1.1,30.779,M,-2.801,M,5.2,0129*6C"
        (list "$GPVTG,343.22,T,,M,5.89,N,10.91,K,D*01"
              "$GPHDT,343.508,T*3C"
              "!GPGGA,092911.40,2228.52767535,N,11352.02427511,E,2,08,1.1,30.777,M,-2.801,M,5.4,0129*6C")
        (list "$GPVTG,343.75,T,,M,5.87,N,10.87,K,D*0A"
              "$GPHDT,343.508,T*3C")
        "$GPGGA,092911.60,2228.52798684,N,11352.02417394,E,2,08,1.1,30.768,M,-2.801,M,5.6,0129*63"
        (list "$GPVTG,343.09,T,,M,5.84,N,10.82,K,D*07"
              "$GPHDT,343.504,T*30")
        "$GPGGA,092911.80,2228.52829844,N,11352.02407914,E,2,08,1.1,30.783,M,-2.801,M,5.8,0129*62"
        (list "$GPVTG,343.85,T,,M,5.89,N,10.90,K,D*0D"
              "$GPHDT,343.486,T*3B")
        "$GPGGA,092912.00,2228.52861357,N,11352.02397610,E,2,08,1.1,30.783,M,-2.801,M,6.0,0129*62"
        (list "$GPVTG,342.95,T,,M,5.91,N,10.95,K,D*01"
              "$GPHDT,343.449,T*38")
        "$GPGGA,092912.20,2228.52892824,N,11352.02387793,E,2,08,1.1,30.775,M,-2.801,M,6.2,0129*63"
        (list "$GPVTG,343.63,T,,M,5.89,N,10.91,K,D*04"
              "$GPHDT,343.490,T*3C")
        "$GPGGA,092912.40,2228.52923796,N,11352.02377869,E,2,08,1.1,30.776,M,-2.801,M,6.4,0129*68"
        (list "$GPVTG,343.26,T,,M,5.80,N,10.73,K,D*00"
              "$GPHDT,343.468,T*3B")))

(define (server)
  (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let ([listener (tcp-listen 18030 256 #true)])
                (define-values (hostname port _r _p) (tcp-addresses listener #true))
                (printf "> ~a:~a~n" hostname port)
                (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener))

                (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                (printf "Greetings, ~a:~a.~n" remote rport)

                (for ([batch (in-list nmeas)])
                  (for ([nmea (if (list? batch) (in-list batch) (in-value batch))])
                    (printf "[~a bytes] ~a~n" (+ (string-length nmea) 2) nmea)
                    (fprintf /dev/tcpout nmea)
                    (write-char #\return /dev/tcpout)
                    (write-char #\linefeed /dev/tcpout))
                  (flush-output /dev/tcpout)
                  (sleep 0.5))))
       (thunk (custodian-shutdown-all (current-custodian))))))
  (sleep 1)
  (server))

(with-handlers ([exn? void])
  (server))
