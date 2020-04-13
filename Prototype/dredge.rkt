#lang racket

(require "gps20130304.rkt")
(require "ais20200323.rkt")
(require "ch6000m3.rkt")

(define gps (thread (λ [] (gps-server 0.1))))
(define ais (thread (λ [] (ais-station 1.0))))
(define plc (thread (λ [] (plc-slaver 1.0))))

(with-handlers ([exn? void])
  (thread-wait gps))

(kill-thread gps)
(kill-thread ais)
(kill-thread plc)
