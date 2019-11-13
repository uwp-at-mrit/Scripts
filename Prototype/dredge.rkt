#lang racket

(require "gps20130304.rkt")
(require "ch6000m3.rkt")

(define gps (thread gps-server))
(define plc (thread plc-slaver))

(with-handlers ([exn? void])
  (thread-wait gps))

(kill-thread gps)
(kill-thread plc)
