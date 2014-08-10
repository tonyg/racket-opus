#lang racket/base
;; libspeexdsp, http://www.speex.org/
;; The DSP library is not yet obsoleted by Opus; only the codec is obsoleted.

(provide speex-echo-state?
	 speex-echo-state-frame-size
	 speex-echo-state-filter-length
	 make-speex-echo-state
	 speex-echo-cancellation!
	 speex-echo-capture!
	 speex-echo-playback!
	 speex-echo-state-reset!)

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/cvector)
(require ffi/vector)

(define speexdsp-lib (ffi-lib "libspeexdsp" '("1" #f)))

(define-ffi-definer define-speexdsp speexdsp-lib #:default-make-fail make-not-available)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-speexdsp speex_echo_state_init (_fun _int _int -> _pointer))
(define-speexdsp speex_echo_state_destroy (_fun _pointer -> _void))
(define-speexdsp speex_echo_cancellation (_fun _pointer _pointer _pointer _pointer -> _void))
(define-speexdsp speex_echo_capture (_fun _pointer _pointer _pointer -> _void))
(define-speexdsp speex_echo_playback (_fun _pointer _pointer -> _void))
(define-speexdsp speex_echo_state_reset (_fun _pointer -> _void))

(struct speex-echo-state (frame-size filter-length pointer))

(define (make-speex-echo-state frame-size [filter-length (* frame-size 10)])
  (define p (speex_echo_state_init frame-size filter-length))
  (define h (speex-echo-state frame-size filter-length p))
  (register-finalizer h (lambda (h) (speex_echo_state_destroy (speex-echo-state-pointer h))))
  h)

(define (speex-echo-cancellation! h recorded-samples played-back-samples)
  (check-frame-size! h recorded-samples)
  (check-frame-size! h played-back-samples)
  (define output (make-output-buffer h))
  (speex_echo_cancellation (speex-echo-state-pointer h) recorded-samples played-back-samples output)
  output)

(define (speex-echo-capture! h recorded-samples)
  (check-frame-size! h recorded-samples)
  (define output (make-output-buffer h))
  (speex_echo_capture (speex-echo-state-pointer h) recorded-samples output)
  output)

(define (speex-echo-playback! h played-back-samples)
  (check-frame-size! h played-back-samples)
  (speex_echo_playback (speex-echo-state-pointer h) played-back-samples))

(define (speex-echo-state-reset! h)
  (speex_echo_state_reset (speex-echo-state-pointer h)))

(define (check-frame-size! h bs)
  (define actual (bytes-length bs))
  (define expected-samples (speex-echo-state-frame-size h))
  (define expected-bytes (* 2 expected-samples))
  (when (not (= actual expected-bytes))
    (error 'speex
	   "Frame size ~a bytes does not match configured frame size of ~a bytes / ~a samples"
	   actual
	   expected-bytes
	   expected-samples)))

(define (make-output-buffer h)
  (make-bytes (* 2 (speex-echo-state-frame-size h))))
