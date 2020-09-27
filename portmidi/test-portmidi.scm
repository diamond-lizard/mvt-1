(require "portmidi-ffi-interface.scm")

;;  This tests the the low-level connection to MIDI via PortMidi

(define (main) 
  (define in #f)
  (define out #f)
  (printf "Here are the MIDI devices currently connected\n")
  (display-midi-devices)
  (printf "Enter a MIDI INPUT device number to test (or #f): ")
  (set! in (read))
  (printf "Enter a MIDI OUTPUT device number to test (or #f): ")
  (set! out (read))
  (midi-open in out midi-receive)
  (when out (test-output))
  (when in (test-input))
  (midi-close)
  (printf "Done testing.\n"))

(define (test-output)
  (printf "\nTesting output.  The C major scale... ") (flush-output)
  (for/list ((note '(60 62 64 65 67 69 71 72)))
    (message #x90 note 90)
    (sleep .25)
    (message #x90 note 0))
  (printf "done.\n"))

(define (midi-receive rtime x y z)
  (when testing? (printf "MIDI test input received at ~s: ~s ~s ~s\n" 
						 rtime x y z)))

(define testing? #f)
(define (test-input)
  (printf "\nTesting input.  Send some notes from MIDI in. ")
  (printf "Type a RETURN to stop.\n")
  (set! testing? #t) (read-char) (read-char)
  (set! testing? #f))
