#lang racket

;;; This provides a low-level interface to MIDI input and output
;;;   via the compiled portmidi-ffi-interface dynamic library

(provide midi-open midi-close message display-midi-devices set-reroute)
(require scheme/foreign)
(unsafe!)

;; change this line to point to the dynamic library file
(define pmlib (ffi-lib "portmidi-ffi-interface.dylib"))

(define (ffi-load name format) 
  (get-ffi-obj name pmlib format (lambda () (error "Load error: ~s" name))))

(define stop-midi (ffi-load "StopMidi" (_fun -> _void)))
(define close-midi-in (ffi-load "CloseMidiIn" (_fun -> _void)))
(define close-midi-out (ffi-load "CloseMidiOut" (_fun -> _void)))
(define open-midi-in (ffi-load "OpenMidiIn" (_fun _int -> _void)))
(define open-midi-out (ffi-load "OpenMidiOut" (_fun _int -> _void)))
(define message (ffi-load "MidiMessageOut" (_fun _int _int _int -> _void)))
(define set-reroute (ffi-load "SetReroute" (_fun _int -> _void)))
(define display-midi-devices (ffi-load "ListDevices" (_fun -> _void)))
(define start-midi
  (ffi-load "StartMidi" 
    (_fun (_fun #:async-apply (lambda (f) (f)) _int _int _int _int -> _void)
          -> _void)))

(define midi-in-num #f)
(define midi-out-num #f)
(define (default-reader rt x y z) (printf "MIDI Received: ~s ~s ~s\n" x y z))

(define (midi-open in out callbackfn)
  (set! midi-in-num in)
  (set! midi-out-num out)
  (start-midi (or callbackfn default-reader))
  (when midi-in-num (open-midi-in in))
  (when midi-out-num (open-midi-out out)))

(define (midi-close)
  (when midi-in-num (close-midi-in))
  (when midi-out-num (close-midi-out))
  (stop-midi))
