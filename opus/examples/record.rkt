#lang racket

(require (only-in file/sha1 bytes->hex-string))
(require portaudio)
(require "../main.rkt")

(define f (open-output-file "tmp-out.hex" #:exists 'replace))

(define start-time (current-inexact-milliseconds))
(define s (pa-default-input-stream 2 'paInt16 48000 #f #f))
(define e (make-opus-encoder 48000 2 'opus-application-voip))

(pa-start-stream s)

(let loop ()
  (when (< (current-inexact-milliseconds) (+ start-time 3000))
    (define-values (overflowed? bs) (pa-read-stream s 480))
    (display (bytes->hex-string (opus-encode e bs)) f)
    (newline f)
    (loop)))

(pa-close-stream s)

(close-output-port f)
