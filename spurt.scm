#lang racket

;;;  This is a version of SPUR that uses a global (bar beat unit) MIDI timing
;;;  and a PortMidi interface for MIDI in and out

(require (lib "defmacro.ss") "midi-io.scm")
(provide (except-out (all-from-out "midi-io.scm") define-time-converter)
     *bpm* *beat* *bar* now bar-beat-unit on-bar? on-beat? next-bar next-beat 
     real->integer rand set-tempo set-time-signature
     initialize-schedule execute-schedule 
     schedule schedule* map-schedule duration location)

(define rnow current-inexact-milliseconds)  ; milli clock 
(define default-end-wait 30)                ; millis to wait before stopping

;; Two useful Impromptu functions
(define (real->integer x) (inexact->exact (round x)))
(define (rand . args)                ; this is called "random" in Impromptu 
  (if (null? args) (random)
      (if (null? (cdr args)) (list-ref (car args) (random (length (car args))))
          (+ (car args) (random (- (cadr args) (car args)))))))

(define *beat*  960)                        ; num of units per beat
(define *bar* 3840)                         ; num of units per bar
(define (set-time-signature num denom)      ; initialize *bar*
  (set! *bar* (* *beat* num)))

;; conversion of bar:beat:unit -> units  (using *beat* and *bar*)
(define (duration* bar beat unit) (+ (* *bar* bar) (* *beat* beat) unit))
(define (duration . args)
  (if (null? (cdr args)) (car args)
      (if (null? (cddr args)) (duration* 0 (car args) (cadr args))
          (apply duration* args))))
(define (location bar beat unit) (duration (- bar 1) (- beat 1) unit))

(define *bpm*  120.0)                       ; num of beats per minute 
(define *factor* .52083)                    ; see the formula below
(define (set-tempo bpm)                     ; initialize *bpm* and *factor*
  (set! *bpm* bpm)
  (set! *factor* (/ 60000.0 (* *bpm* *beat*))))

;; conversion of unit time <-> real time (using *factor*)
(define (utime->rtime units) (* *factor* units))
(define (rtime->utime ms) (real->integer (/ ms *factor*)))
(define-time-converter                      ; this is for recorded events
  (lambda (rtime)
    (+ prev-event-utime (rtime->utime (- rtime prev-event-rtime)))))

;; operations on utime
(define (now) next-event-utime)
(define (bar-beat-unit utime)
  (let ((bar (truncate (/ utime *bar*)))
        (beat (truncate (/ (remainder utime *bar*) *beat*)))
        (unit (remainder utime *beat*)))
    (format "~s:~s:~s" (+ 1 bar) (+ 1 beat) unit)))
(define (on-bar? utime) (= 0 (remainder utime *bar*)))
(define (on-beat? utime) (= 0 (remainder utime *bar*)))
(define (next-bar utime) (+ utime (- *bar* (remainder utime *bar*))))
(define (next-beat utime) (+ utime (- *beat* (remainder utime *beat*))))

;; an event consists of a utime, a thunk, and a next event (or #f)
(define-macro (make-event utime thk next) `(mcons (cons ,utime ,thk) ,next))
(define-macro (ev-utime ev) `(car (mcar ,ev)))
(define-macro (ev-thunk ev) `(cdr (mcar ,ev)))
(define-macro (ev-next ev) `(mcdr ,ev))
(define-macro (set-ev-pair! ev val) `(set-mcar! ,ev ,val))
(define-macro (set-ev-next! ev val) `(set-mcdr! ,ev ,val))

;; two pointers into the schedule
(define head-event #f)           ; the front of the schedule
(define tail-event #f)           ; somewhere near the rear

;; assigned during the schedule execution
(define prev-event-utime 0)      ; the utime of the previous event
(define prev-event-rtime (rnow)) ; the rtime of the previous event
(define next-event-utime 0)      ; the utime of the first event
(define next-event-rtime 0)      ; the rtime of the first event
(define waiting? #f)             ; paused in the executor?
(define pause-chan #f)           ; channel for releasing pauses

;; initialize the two event pointers and the execution variables
(define (initialize-schedule) 
  (collect-garbage) (set! next-event-utime 0)
  (set! head-event #f) (set! tail-event #f)
  (set! waiting? #f) (set! pause-chan (make-channel)))

;; sleep until next-event-rtime or until awakened by schedule*
(define (pause) 
  (set! waiting? #t)
  (sync (alarm-evt next-event-rtime) pause-chan)
  (set! waiting? #f))

;; execute each event in the schedule at its proper time
(define (execute-schedule [end-wait default-end-wait])
  (collect-garbage)
  (unless pause-chan (error "Schedule was not initialized"))
  (set! prev-event-utime (if head-event (ev-utime head-event) 0))
  (set! prev-event-rtime (rnow))
  (let loop ((time (rnow)))
    (if head-event                      ; any events in the schedule?
        (begin
          (set! next-event-utime (ev-utime head-event))  ; scheduled utime
          (set! next-event-rtime               ; calculate scheduled rtime
                (+ prev-event-rtime 
                   (utime->rtime (- next-event-utime prev-event-utime))))
          (if (> next-event-rtime time)  (pause)          ; try later?
              (let ((thunk (ev-thunk head-event)))        ; else now
                (set! head-event (ev-next head-event))
                (thunk)   ; do the thunk live    
                (set! prev-event-utime next-event-utime)
                (set! prev-event-rtime next-event-rtime)))
          (loop (rnow)))
        (begin                          ; last chance before stopping
          (set! next-event-rtime (+ time end-wait))
          (pause)                       ; allow for outstanding events
          (if head-event (loop (rnow)) (set! pause-chan #f))))))

;; create an event, insert it into its proper place in the schedule,
;; and wake a paused executor if the new event goes first
(define (schedule* utime thunk)
  ; first adjust tail-event when head-event has moved beyond it
  (when (or (not head-event)
            (and tail-event (> (ev-utime head-event) (ev-utime tail-event))))
        (set! tail-event head-event))
  (let ((new (make-event utime thunk #f)))
    ; a loop to search for the correct position for the new event
    (define (search-loop ev1 ev2)     ; ev1 < ev2  and  ev1 < utime
      (if (and ev2 (> utime (ev-utime ev2))) 
          (search-loop ev2 (ev-next ev2))
          (begin (set! tail-event ev1)
                 (set-ev-next! new ev2)        
                 (set-ev-next! ev1 new))))
    (if (and tail-event (> utime (ev-utime tail-event)))
        (search-loop tail-event (ev-next tail-event))
        (if (and head-event (> utime (ev-utime head-event)))
            (search-loop head-event (ev-next head-event))
            (begin (set-ev-next! new head-event) 
                   (set! head-event new)))))
  (when (and waiting? (< utime next-event-utime))  ; if new event is now first 
        (channel-put pause-chan #t)))              ; release paused executor

(define-macro (schedule utime fn . args) 
   `(schedule* ,utime (lambda () (,fn ,@args))))

;; map schedule to a list of (utime,thunk) pairs, applying functions to each
(define (map-schedule utime-fn thunk-fn)
  (define ufn (or utime-fn (lambda (x) x)))
  (define tfn (or thunk-fn (lambda (x) x)))
  (let loop ((ev head-event))
    (if (not ev) '()
        (cons (cons (ufn (ev-utime ev)) (tfn (ev-thunk ev)))
              (loop (ev-next ev))))))
