#lang racket

(provide plc-slaver)

(require "message.rkt")
(require "../catalogue/tongue.rkt")
(require "../../CH6000m3/SCADA/stone/tongue/alarm.resw.rkt")

(require racket/tcp)
(require racket/generator)

(require syntax/location)

(define ch6000m3.plc
  (with-handlers ([exn? (λ [e] null)])
    (call-with-input-file (path-replace-extension (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)) #".plc")
      (λ [/dev/plcin]
        (let port->plc ([sclp null])
          (define timepoint (read /dev/plcin))
          (cond [(eof-object? timepoint) (reverse sclp)]
                [else (let ([addr0 (read /dev/plcin)]
                            [addrn (read /dev/plcin)])
                        (read-line /dev/plcin 'return-linefeed)
                        (port->plc (let ([plc (read-bytes (add1 (- addrn addr0)) /dev/plcin)])
                                     (read-line /dev/plcin 'return-linefeed)
                                     (cons plc sclp))))]))))))

(define memory (make-bytes #x1264))
(define master-ipv4 (with-handlers ([exn? (λ [_] #false)]) (vector-ref (current-command-line-arguments) 0)))
(define master-port 2008)

(define db4 4122)
(define db205 4322)

(define set-digital-input
  (case-lambda
    [(offset index)
     (define-values (q r) (quotient/remainder (- index 1) 8))
     (set-digital-input offset q r)]
    [(offset index bindex)
     (bytes-set! memory (+ offset index)
                 (bitwise-ior (bytes-ref memory (+ offset index))
                              (arithmetic-shift #x1 bindex)))]))

(define clear-digital-input
  (case-lambda
    [(offset index)
     (define-values (q r) (quotient/remainder (- index 1) 8))
     (clear-digital-input offset q r)]
    [(offset index bindex)
     (bytes-set! memory (+ offset index)
                 (bitwise-and (bytes-ref memory (+ offset index))
                              (bitwise-not (arithmetic-shift #x1 bindex))))]))

(define refresh-memory
  (let ([plc (sequence->repeated-generator ch6000m3.plc)])
  (lambda [alarms4 alarms205]
    (cond [(null? ch6000m3.plc) (bytes-fill! memory 0)]
          [else (displayln "use history snapshot")
                (bytes-copy! memory 0 (plc) 0 (bytes-length memory))])

    ;;; DB2
    (when (null? ch6000m3.plc)
      (for ([i (in-range 1 176)]) ;; don't change the tidemark
        (real->floating-point-bytes (+ 2.0 (random)) 4 #true memory (+ 3418 (* i 4)))))
    
    ;;; DB4
    (when (null? ch6000m3.plc)
      (for ([i (in-range 124)])
        (unless (< 40 i 50) ;; winches and gantries
          (define state (arithmetic-shift #x1 (random 8)))
          (bytes-set! memory (+ db4 i) state))))

    ;; gantries
    (set-digital-input db4 42 4)
    (set-digital-input db4 48 5)
    
    ;; alarms
    (for ([idx (in-list alarms4)])
      (clear-digital-input db4 (add1 idx)))

    ;(set-digital-input db4 (add1 (car alarms4)))
    
    ;;; DB203
    (when (null? ch6000m3.plc)
      (for ([i (in-range 280)])
        (real->floating-point-bytes (+ 203.0 (random)) 4 #true memory (+ 1120 (* i 4)))))

    ;;; DB205
    ;; gantries, ps trunnion - sb draghead
    (for ([dbx (in-range 163 169)])
      (set-digital-input db205 dbx (random 3)))
    
    ;; winches
    (for ([dbx (in-range 169 174)])
      (set-digital-input db205 dbx (random 8)))
    (set-digital-input db205 174 4)
    (set-digital-input db205 174 5)
    
    ;; alarms
    (for ([idx (in-list alarms205)])
      (clear-digital-input db205 (add1 idx))))))

(define-values (alarms4 alarms205)
  (let-values ([(classname data) (alarm-tongues)])
    (for/fold ([a4 null] [a205 null])
              ([tongue-index (map tongue-index data)])
      (define-values (db idx) (alarm-db-index tongue-index))
      (cond [(= db 4) (values (cons idx a4) a205)]
            [else (values a4 (cons idx a205))]))))

(define (wait-read-response-loop /dev/tcpin /dev/tcpout remote rport)
  (define-values (signature data) (read-mrmsg /dev/tcpin 40))
  (define-values (db addr0 addrn) (values (mrmsg-block signature) (mrmsg-addr0 signature) (mrmsg-addrn signature)))
  
  (case (mrmsg-code signature)
    [(#x41) (refresh-memory alarms4 alarms205)
            (printf ">> [sent ~a bytes to ~a:~a]~n"
                    (write-mrmsg /dev/tcpout (mrmsg-code signature) (mrmsg-block signature) addr0 addrn memory)
                    remote rport)]
    [else (void)])
  (wait-read-response-loop /dev/tcpin /dev/tcpout remote rport))

(define (plc-slaver)
  (let connect-send-wait-loop ()
    (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
      (parameterize ([current-custodian (make-custodian)])
        (dynamic-wind
         (thunk (void))
         (thunk (if (string? master-ipv4)
                    (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break master-ipv4 master-port)])
                      (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                      (printf "[connected to ~a:~a]~n" remote rport)
                      (wait-read-response-loop /dev/tcpin /dev/tcpout remote rport))
                    (let ([listener (tcp-listen master-port)])
                      (define-values (hostname port _r _p) (tcp-addresses listener #true))
                      (printf "> PLC:~a:~a~n" hostname port)
                      
                      (let-values ([(/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener)])
                        (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                        (printf "[accepted ~a:~a]~n" remote rport)
                        (wait-read-response-loop /dev/tcpin /dev/tcpout remote rport)))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))

(module+ main
  (with-handlers ([exn? void])
    (plc-slaver)))
