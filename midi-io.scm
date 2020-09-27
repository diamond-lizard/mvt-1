#lang racket

;;;  This provides a high-level MIDI interface for input and output.
;;;  MIDI out is provided via note-on, note-off, control-change, etc.
;;;  MIDI in is provided via start-recording and stop-recording 
;;;    which produces a file of MIDI events in CSV format.  
;;;  The define-time-converter provides a way to specify a function 
;;;    to convert rtime to utime (according to tempo) during MIDI recording

(require (file "portmidi-ffi-interface.scm"))

(provide 
     open-midi-devices close-midi-devices display-midi-devices 
     note-off note-on aftertouch control-change pitch-bend program-change
	 start-recording stop-recording define-time-converter)

(define (open-midi-devices input-device output-device)
  (set! porttime-offset (current-inexact-milliseconds))
  (midi-open input-device output-device 
     (lambda (rtime x y z) (when recording? (receive-input rtime x y z)))))
(define (close-midi-devices) (midi-close))
(define (time-converter ms) (error "Time conversion not initialized"))
(define (define-time-converter fn) (set! time-converter fn))

(define ON #x90)  ; note on
(define OFF #x80) ; note off
(define AT #xA0)  ; aftertouch
(define PC #xC0)  ; program change 
(define CC #xB0)  ; control change
(define PB #xE0)  ; pitch bend
(define MIDI-CODE-MASK #xF0)
(define MIDI-CHAN-MASK #x0F)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Sending MIDI bytes

(define (note-on channel note (velocity 64))
  (message (bitwise-ior ON (- channel 1)) note velocity))

(define (note-off channel note (velocity 64))
  (message (bitwise-ior OFF (- channel 1)) note velocity))

(define (aftertouch channel note touch)
  (message (bitwise-ior AT (- channel 1)) note touch))

(define (program-change channel preset)
  (message (bitwise-ior PC (- channel 1)) preset 0))

(define (control-change channel controlnum value)
  (message (bitwise-ior CC (- channel 1)) controlnum value))

(define (pitch-bend channel value)
  (message (bitwise-ior PB (- channel 1))
		   (bitwise-and value 127) (arithmetic-shift value -7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Receiving MIDI bytes

(define csv-file "/tmp/track.csv")
(define csv-port #f)
(define reroute 0)
(define recording? #f)
(define end-utime 0)
(define porttime-offset 0)

;; Start recording MIDI events, possible rerouting them
(define (start-recording [reroute 0])
  (set-reroute reroute)
  (set! csv-port (open-output-file csv-file #:mode 'text #:exists 'replace))
  (fprintf csv-port "1, 0, Start_track\n")
  (set! recording? #t))

;; Stop recording.  The recorded events are in /tmp/track.csv.
(define (stop-recording)
  (set! recording? #f)
  (fprintf csv-port "1, ~s, End_track\n" end-utime)
  (close-output-port csv-port)
  (set! csv-port #f))

;; Write to the file /tmp/track.csv as MIDI data arrives
(define (receive-input rtime status data1 data2)
  (set! end-utime (time-converter (+ rtime porttime-offset)))
  (fprintf csv-port "1, ~s, " end-utime)
  (define command (bitwise-and status MIDI-CODE-MASK))
  (define channel (bitwise-and status MIDI-CHAN-MASK))
  (cond
   ((and (= command ON) (not (= 0 data2)))
	   (fprintf csv-port "Note_on_c, ~s, ~s, ~s\n" channel data1 data2))
   ((or (= command ON) (= command OFF))
	   (fprintf csv-port "Note_off_c, ~s, ~s, ~s\n" channel data1 data2))
   ((= command CC)
	   (fprintf csv-port "Control_c, ~s, ~s, ~s\n" channel data1 data2))
   ((= command PC)
	   (fprintf csv-port "Program_c, ~s, ~s\n" channel data1))
   ((= command AT)
	   (fprintf csv-port "Channel_aftertouch_c, ~s, ~s\n" channel data1))
   ((= command PB)
	   (fprintf csv-port "Pitch_bend_c, ~s, ~s\n" channel 
			(bitwise-ior data1 (arithmetic-shift data2 7))))	
   (else (error "MVT: MIDI input ~s not handled" command))))
