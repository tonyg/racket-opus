#lang racket

(require (only-in file/sha1 hex-string->bytes))
(require portaudio)
(require "../main.rkt")

(define f (open-input-file "tmp-out.hex"))

(define s (pa-default-output-stream 2 'paInt16 12000 #f #f))
(define d (make-opus-decoder 12000 2))

(pa-start-stream s)

(let loop ()
  (define line (read-line f))
  (when (not (eof-object? line))
    (define packet (hex-string->bytes line))
    (define frame (opus-decode d packet))
    (pa-write-stream s frame)
    (loop)))

(pa-close-stream s)

(close-input-port f)
