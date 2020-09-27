#lang racket

;;;  Play and Record MVT files using SPURT

(require mzlib/string (lib "defmacro.ss") (file "spurt.scm"))
(provide (all-from-out (file "spurt.scm"))
   start-clicking stop-clicking start-playing stop-playing 
   start-tracking stop-tracking play-event schedule-file main)

;; this main procedure can be used to play an entire MVT file uninterrupted
;; Usage:  racket -tm mvt-spurt.scm <mvt file>
(define (main . strs)
  (when (null? strs) (error "No MVT file to play"))
  (schedule-file (car strs))   ; schedule the file for playing
  (open-midi-devices #f 0)     ; open the first MIDI output device
  (start-playing)              ; start executing the schedule
  (thread-wait play-thread))   ; wait until it stops

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Parsing of location, duration, pitch, and controllers in MVT files

;; white note characters -> MIDI values (0 to 11)
(define whites (hash #\C 0 #\D 2 #\E 4 #\F 5 #\G 7 #\A 9 #\B 11))

;; translate a symbol like Eb3 to a number 
(define (get-pitch sym) 
  (let* ((str (symbol->string sym))
         (acc (if (regexp-match "b" str) -1 (if (regexp-match "#" str) 1 0)))
         (wh (hash-ref whites (string-ref str 0)))
         (oct (string->number (substring str (if (= acc 0) 1 2)))))
    (+ (* 12 (+ oct 1)) wh acc)))

;; read the MIDI controller names and make a hash table
(define midi-controls
  (with-input-from-file "MIDIControllers.txt"
    (lambda () 
      (let ((h (make-hasheq)))
        (let loop ((num (read)))
          (unless (eof-object? num)
            (hash-set! h (read) num)
            (loop (read))))
        h))))

;; translate a symbol like Pan_position or MIDI_Controller_41 to a number
(define (get-controller sym)
  (or (hash-ref midi-controls sym #f)
      (car (reverse (regexp-split #rx"_" (symbol->string sym))))))

;; translate a symbol like 16:2:0 or 3:120 or just 240 into a list of numbers
(define (parse-dur arg)
  (if (number? arg) (list arg)
      (map string->number (regexp-split #rx":" (symbol->string arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Reading of MVT files to produce a schedule

;; a list obtained from the next line of input
(define (read-list-line) 
  (let ((inp (read)))
    (if (eof-object? inp) '() (cons inp (read-from-string-all (read-line))))))

;; Read an MVT file, scheduling events, but not before START-BAR (default 1)
(define (schedule-file file [start-bar 1])
  (with-input-from-file file
    (lambda ()   ; go through the file line by line
      (let loop ((inp (read-list-line)))
        (unless (null? inp)
          (case (car inp)
            ((end-file end-track) #f)
            ((start-file)
               (set! soloing? (memq 'soloing (cdr inp)))
               (set-time-signature (cadr inp) (caddr inp))
               (set! start-utime (location start-bar 1 0))
               (set-tempo 120.0)  ; default for an MVT file
               (initialize-schedule))
            ((start-track)
               (set! track-name (cadr inp))
               (set! track-soloed? (memq 'solo (cdr inp)))
               (set! track-muted? (memq 'mute (cdr inp))))
            (else (parse-event inp track-schedule)))
          (loop (read-list-line)))))))

;; Parse the event given by list EV and call SCHEDFN to schedule the result.
;; Note: nothing is done for meta events.
(define (parse-event ev schedfn)
  (define loc (apply location (parse-dur (car ev))))
  (define type (cadr ev))
  (define fields (cddr ev))
  (case type 
    ((tempo) (schedfn loc set-tempo (car fields)))
    ((note)
       (let-values (((chan pitchsym vel dursym) (apply values fields)))
         (define pitch (get-pitch pitchsym))
         (define dur (apply duration (parse-dur dursym)))
         (when (and tracking? (string=? track-name tracking?))
            (schedfn (- loc 10) displayln (car ev)))
         (schedfn loc note-on chan pitch vel)
         (schedfn (+ loc dur -2) note-off chan pitch)))
    ((sustain volume expression wheel)
       (define ctl 
         (case type ((sustain) 64) ((volume) 7) ((expression) 11) ((wheel) 1)))
       (let-values (((chan amount) (apply values fields)))
         (schedfn loc control-change chan ctl amount)))
    ((control)
       (let-values (((chan ctlsym amount) (apply values fields)))
         (define ctl (get-controller ctlsym))
         (schedfn loc control-change chan ctl amount)))
    ((program bend after)
       (define prog (case type ((program) program-change) 
                          ((bend) pitch-bend) ((after) aftertouch)))
       (let-values (((chan amount) (apply values fields)))
         (when (eq? type 'bend) (set! amount (- amount 8192)))
         (schedfn loc prog chan amount)))))
 
(define soloing? #f)           ; should a track be quiet unless soloed?
(define track-soloed? #f)      ; is the track soloed?
(define track-muted? #f)       ; is the track muted?
(define track-name #f)         ; the name of the current track
(define start-utime 0)         ; first utime to schedule

;; schedule an event unless mute, time, or solo indicate otherwise
(define (track-schedule utime fn . args)
  (unless (or track-muted? (< utime start-utime)
              (and soloing? (not track-soloed?)))
     (case (length args)
       ((1) (schedule utime fn (car args)) )
       ((2) (schedule utime fn (car args) (cadr args)) )
       ((3) (schedule utime fn (car args) (cadr args) (caddr args)) ))))

;; play the events in the schedule in a separate thread
(define play-thread #f)        ; thread for playing 
(define (all-notes-off) (for ([i 16]) (control-change (+ i 1) 123 0)))
(define (start-playing) (set! play-thread (thread execute-schedule)))
(define (stop-playing) (kill-thread play-thread) (all-notes-off))

;; play a single MVT event given by the string STR immediately
(define (play-event str) 
  (parse-event (read-from-string-all str) 
    (lambda (utime fn . args) (apply fn args) 
        (when (eq? fn note-on) (sleep .25)))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Handle playing a click and tracking

;; control the starting and stopping of printing timing info
(define tracking? #f)
(define (start-tracking trname) (set! tracking? trname))
(define (stop-tracking) (set! tracking? #f))

;; control the starting and stopping of clicking on the beat
(define clicking? #f)
(define (start-clicking) 
  (set! clicking? #t) 
  (click (next-beat (max (now) start-utime))))
(define (stop-clicking) (set! clicking? #f))

;; This defines the clicking sounds. hi for downbeat, lo for others.
;; Options other than notes are (system "echo '\007'") or (play-sound <file>)
(define (hi-click-sound) (note-on 10 76 110))  ; woodblock at 110
(define (lo-click-sound) (note-on 10 76  40))  ; woodblock at 40
(define (no-click-sound) (note-off 10 76))     ; woodblock at 0

;; schedule click sounds to occur on every beat while clicking? is true
(define (click utime)
  (when clicking? 
    (schedule* utime (if (on-bar? utime) hi-click-sound lo-click-sound))
    (schedule* (+ utime 10) no-click-sound)
    (schedule (+ utime *beat* -100) click (+ utime *beat*))))
