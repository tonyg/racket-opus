#lang racket
;; Opus, http://www.opus-codec.org/

(provide (struct-out encoder-state)
	 (struct-out decoder-state)

	 make-opus-encoder
	 opus-encode
	 opus-encode-float

	 make-opus-decoder
	 opus-decode
	 opus-decode-float

	 opus-encoder-set-bitrate!
	 opus-encoder-get-bitrate)

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/cvector)
(require ffi/vector)

(define opus-lib (ffi-lib "libopus" '("0" #f)))

(define-ffi-definer define-opus opus-lib #:default-make-fail make-not-available)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-frame-durations ;; in milliseconds
  '(2.5 5 10 20 40 60))

(define (ms->frame-size e sample-size ms)
  (* sample-size
     (encoder-state-num-input-channels e)
     (encoder-state-input-sample-rate e)
     (/ ms 1000.0)))

(define (frame-size-valid? e frame sample-size)
  (ormap (lambda (ms) (= (bytes-length frame) (ms->frame-size e sample-size ms)))
	 valid-frame-durations))

(define (ensure-valid-frame-size function-name e frame sample-size)
  (when (not (frame-size-valid? e frame sample-size))
    (error function-name
	   "Invalid frame size ~a; valid sizes (for ~a channel(s), sample rate ~aHz) are: ~a"
	   (bytes-length frame)
	   (encoder-state-num-input-channels e)
	   (encoder-state-input-sample-rate e)
	   (map (lambda (ms) (ms->frame-size e sample-size ms)) valid-frame-durations))))

(define (opus-ok? error-code)
  (zero? error-code))

(define-opus opus_strerror (_fun _int -> _string))

(define (die-if-error function-name error-code)
  (if (negative? error-code)
      (error function-name "Opus error: ~a (code ~a)" (opus_strerror error-code) error-code)
      error-code))

(define _coding-mode (_enum '(opus-application-voip = 2048
			      opus-application-audio
			      opus-application-restricted-lowdelay)))

(define-opus opus_encoder_get_size (_fun _int -> _int))
(define-opus opus_encoder_init (_fun _pointer _int32 _int _coding-mode -> _int))

(struct encoder-state (bytes input-sample-rate num-input-channels coding-mode) #:prefab)

(define (make-opus-encoder input-sample-rate num-input-channels coding-mode)
  (define bs (make-bytes (opus_encoder_get_size num-input-channels) 0))
  (die-if-error 'make-opus-encoder (opus_encoder_init bs input-sample-rate num-input-channels coding-mode))
  (encoder-state bs input-sample-rate num-input-channels coding-mode))

(define-opus opus_encode (_fun _pointer _bytes _int _bytes _int32 -> _int32))

;; Expects little-endian 16-bit signed ints as a byte vector
(define (opus-encode state input-samples #:output-buffer-size-limit [max-data-bytes 4096]) ;; TODO: reasonable limit?
  (ensure-valid-frame-size 'opus-encode state input-samples 2)
  (define buffer (make-bytes max-data-bytes))
  (define actual-packet-size (die-if-error 'opus-encode
					   (opus_encode (encoder-state-bytes state)
							input-samples
							(/ (bytes-length input-samples)
							   (* 2 (encoder-state-num-input-channels state)))
							buffer
							max-data-bytes)))
  ;; TODO: documentation claims that if (= actual-packet-size 1), the packet does not actually need to be transmitted!
  (subbytes buffer 0 actual-packet-size))

(define-opus opus_encode_float (_fun _pointer _bytes _int _bytes _int32 -> _int32))

;; Expects little-endian 32-bit floats as a byte vector
(define (opus-encode-float state input-samples #:output-buffer-size-limit [max-data-bytes 4096]) ;; TODO: reasonable limit?
  (ensure-valid-frame-size 'opus-encode-float state input-samples 4)
  (define buffer (make-bytes max-data-bytes))
  (define actual-packet-size (die-if-error 'opus-encode-float
					   (opus_encode_float (encoder-state-bytes state)
							      input-samples
							      (/ (bytes-length input-samples)
								 (* 4 (encoder-state-num-input-channels state)))
							      buffer
							      max-data-bytes)))
  ;; TODO: documentation claims that if (= actual-packet-size 1), the packet does not actually need to be transmitted!
  (subbytes buffer 0 actual-packet-size))

(define _ctl-request (_enum '(OPUS_SET_APPLICATION_REQUEST =         4000
			      OPUS_GET_APPLICATION_REQUEST =         4001
			      OPUS_SET_BITRATE_REQUEST =             4002
			      OPUS_GET_BITRATE_REQUEST =             4003
			      OPUS_SET_MAX_BANDWIDTH_REQUEST =       4004
			      OPUS_GET_MAX_BANDWIDTH_REQUEST =       4005
			      OPUS_SET_VBR_REQUEST =                 4006
			      OPUS_GET_VBR_REQUEST =                 4007
			      OPUS_SET_BANDWIDTH_REQUEST =           4008
			      OPUS_GET_BANDWIDTH_REQUEST =           4009
			      OPUS_SET_COMPLEXITY_REQUEST =          4010
			      OPUS_GET_COMPLEXITY_REQUEST =          4011
			      OPUS_SET_INBAND_FEC_REQUEST =          4012
			      OPUS_GET_INBAND_FEC_REQUEST =          4013
			      OPUS_SET_PACKET_LOSS_PERC_REQUEST =    4014
			      OPUS_GET_PACKET_LOSS_PERC_REQUEST =    4015
			      OPUS_SET_DTX_REQUEST =                 4016
			      OPUS_GET_DTX_REQUEST =                 4017
			      OPUS_SET_VBR_CONSTRAINT_REQUEST =      4020
			      OPUS_GET_VBR_CONSTRAINT_REQUEST =      4021
			      OPUS_SET_FORCE_CHANNELS_REQUEST =      4022
			      OPUS_GET_FORCE_CHANNELS_REQUEST =      4023
			      OPUS_SET_SIGNAL_REQUEST =              4024
			      OPUS_GET_SIGNAL_REQUEST =              4025
			      OPUS_GET_LOOKAHEAD_REQUEST =           4027
			      OPUS_GET_SAMPLE_RATE_REQUEST =         4029
			      OPUS_GET_FINAL_RANGE_REQUEST =         4031
			      OPUS_GET_PITCH_REQUEST =               4033
			      OPUS_SET_GAIN_REQUEST =                4034
			      OPUS_GET_GAIN_REQUEST =                4045
			      OPUS_SET_LSB_DEPTH_REQUEST =           4036
			      OPUS_GET_LSB_DEPTH_REQUEST =           4037
			      OPUS_GET_LAST_PACKET_DURATION_REQUEST = 4039
			      OPUS_SET_EXPERT_FRAME_DURATION_REQUEST = 4040
			      OPUS_GET_EXPERT_FRAME_DURATION_REQUEST = 4041
			      OPUS_SET_PREDICTION_DISABLED_REQUEST = 4042
			      OPUS_GET_PREDICTION_DISABLED_REQUEST = 4043
			      )))

(define-opus opus_encoder_ctl/int32
  (_fun _pointer _ctl-request _int32 -> _int)
  #:c-id opus_encoder_ctl)

(define-opus opus_encoder_ctl->int32
  (_fun _pointer _ctl-request (output : (_ptr o _int32)) -> (result : _int) -> (values result output))
  #:c-id opus_encoder_ctl)

(define (opus-encoder-ctl/int32 state req arg)
  (die-if-error req (opus_encoder_ctl/int32 (encoder-state-bytes state) req arg)))

(define (opus-encoder-ctl->int32 state req)
  (define-values (result output) (opus_encoder_ctl->int32 (encoder-state-bytes state) req))
  (die-if-error req result)
  output)

(define (opus-encoder-set-bitrate! state new-bitrate)
  (opus-encoder-ctl/int32 state 'OPUS_SET_BITRATE_REQUEST new-bitrate))

(define (opus-encoder-get-bitrate state)
  (opus-encoder-ctl->int32 state 'OPUS_GET_BITRATE_REQUEST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-opus opus_decoder_get_size (_fun _int -> _int))
(define-opus opus_decoder_init (_fun _pointer _int32 _int -> _int))

(struct decoder-state (bytes output-sample-rate num-output-channels) #:prefab)

(define (make-opus-decoder output-sample-rate num-output-channels)
  (define bs (make-bytes (opus_decoder_get_size num-output-channels) 0))
  (die-if-error 'make-opus-decoder (opus_decoder_init bs output-sample-rate num-output-channels))
  (decoder-state bs output-sample-rate num-output-channels))

(define-opus opus_decode (_fun _pointer _bytes _int32 _bytes _int _bool -> _int32))

(define (max-useful-frame-size state)
  ;; Number of frames in 120ms of audio
  (inexact->exact (truncate (* 0.120 (decoder-state-output-sample-rate state)))))

(define (opus-decode state input-data
		     #:frame-size-limit [max-frame-size (max-useful-frame-size state)]
		     #:decode-fec? [decode-fec? #f])
  (define frames (make-bytes (* 2 (decoder-state-num-output-channels state) max-frame-size) 0))
  (define actual-frame-count (die-if-error 'opus-decode
					   (opus_decode (decoder-state-bytes state)
							input-data
							(bytes-length input-data)
							frames
							max-frame-size
							decode-fec?)))
  (subbytes frames 0 (* 2 (decoder-state-num-output-channels state) actual-frame-count)))

(define-opus opus_decode_float (_fun _pointer _bytes _int32 _bytes _int _bool -> _int32))

(define (opus-decode-float state input-data
			   #:frame-size-limit [max-frame-size (max-useful-frame-size state)]
			   #:decode-fec? [decode-fec? #f])
  (define frames (make-bytes (* 4 (decoder-state-num-output-channels state) max-frame-size) 0))
  (define actual-frame-count (die-if-error 'opus-decode
					   (opus_decode_float (decoder-state-bytes state)
							      input-data
							      (bytes-length input-data)
							      frames
							      max-frame-size
							      decode-fec?)))
  (subbytes frames 0 (* 4 (decoder-state-num-output-channels state) actual-frame-count)))
