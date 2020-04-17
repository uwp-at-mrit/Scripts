#lang racket

(require "gps20130304.rkt")
(require "ais20200323.rkt")
(require "ch6000m3.rkt")

(define devices
  (map (λ [device] (string->symbol (string-downcase device)))
       (vector->list (current-command-line-arguments))))

(parameterize ([current-command-line-arguments (vector)])
  (define gps
    (thread (λ [] (when (or (null? devices) (memq 'gps devices))
                    (gps-server 0.1)))))
  
  (define ais
    (thread (λ [] (when (or (null? devices) (memq 'ais devices))
                    (ais-station 0.5)))))
  
  (define plc
    (thread (λ [] (when (or (null? devices) (memq 'plc devices))
                    (plc-slaver 0.2)))))
  
  (with-handlers ([exn? void])
    (sync/enable-break never-evt))
  
  (kill-thread gps)
  (kill-thread ais)
  (kill-thread plc))
