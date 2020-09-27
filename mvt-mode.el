;; Put (autoload 'mvt-mode "/path/to/thisfile" "" t) in your .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Configuration global variables

(defconst mvt-version 1.0  "The current version of this software")
(defconst mvt-racket "/Applications/Racket v5.2/bin/racket"
  "The binary for running Racket. (Be careful about spaces in the name.)")

;; These determine what MIDI events will be visible in a buffer initially
(defconst mvt-file-ini-vis '(meta control program)
  "The MIDI events that will be visible initially in the file buffer")
(defconst mvt-track-ini-vis '(meta control program note)
  "The MIDI events that will be visible initially in a track buffer")

;; These parameters are for the interaction with mvt-spurt for realtime MIDI
(defvar mvt-auto-start-spurt-p nil
  "Should mvt-spurt be started automatically at startup?
   If true, mvt-input-device and mvt-output-device must have correct values")
(defvar mvt-input-device nil
  "If non-NIL, should be a MIDI device number.")
(defvar mvt-output-device 0
  "If non-NIL, should be a MIDI device number.")
(defvar mvt-tempo-bpm 120.
  "The default tempo for the mvt-record command")
(defvar mvt-lead-in 0
  "The number of lead-in bars for the mvt-record command")
(defvar mvt-tracking-p t
  "If true, the cursor in a track buffer will advance while playing")
(defvar mvt-clicking-p t
  "If true, mvt-spurt will click while recording")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Global constants/variables that should not be reset by users
;;;  (Names that start with "!mvt" are mostly for internal use)

(defconst !mvt-ubeat 960 "number of MIDI units per beat")

;; These are given their values by the "start-file" line of the file
(defconst !mvt-numer nil "numerator of time signature = beats per bar")
(defconst !mvt-denom nil "denominator of time signature")
(defconst !mvt-ubar nil "number of MIDI units per bar (via !mvt-ubeat)")

;; These are used to convert to and from note names and the numbers 0-127.
(defconst !mvt-pitch-values 
  '(("C" . 0) ("D" . 2) ("E" . 4) ("F" . 5) ("G" . 7) ("A" . 9) ("B" . 11))
  "Alist with offsets for each white note in an octave")
(defconst !mvt-pitch-string " CDb DEb E FGb GAb ABb B"
  "A string consisting of two-character names for each note in an octave")

;; See the docs/file-format.txt for explanation of these
(defconst !mvt-all-types
  '(note bend sustain volume expression wheel after control program tempo meta)
  "The eleven recognized types of MIDI events")
(defconst !mvt-all-fields
  '(location channel pitch amount duration value)
  "The recognized fields for changing MIDI events in mvt-mode")

(defconst !mvt-default-plist
    '(note          "note 1  C3 100 480"
      bend          "bend 1 0"
      sustain       "sustain 1 127"
      volume        "volume 1 127"
      expression    "expression 1 127"
      wheel         "wheel 1 0"
      after         "after 1 0"
      control       "control 1 Midi_control_3 0"
      program       "program 1 0"
      tempo         "tempo 120.00"
      meta          "meta Text_t \"Some text goes here\"")
    "Default events for each type as a string without location")

(defconst !mvt-field-plist
  (let* ((loc `(location 0 ,#'!mvt-parse-location ,#'!mvt-format-location))
         (chan `(channel 2 ,#'string-to-number ,#'!mvt-format-channel))
         (pitch `(pitch 3 ,#'!mvt-parse-pitch ,#'!mvt-format-pitch))
         (vel `(amount 4 ,#'string-to-number ,#'!mvt-format-velocity))
         (dur `(duration 5 ,#'!mvt-parse-duration ,#'!mvt-format-duration))
         (bend `(amount 3 ,#'string-to-number ,#'!mvt-format-bend))
         (val3 `(value 3 ,#'string-to-number ,#'!mvt-format-value))
         (val4 `(value 4 ,#'string-to-number ,#'!mvt-format-value))
         (amt3 `(amount 3 ,#'string-to-number ,#'!mvt-format-value))
         (amt2 `(amount 2 ,#'string-to-number ,#'!mvt-format-bpm)))
  `(note (,loc ,chan ,pitch ,vel ,dur) bend (,loc ,chan ,bend) 
    sustain (,loc ,chan ,val3) volume (,loc ,chan ,amt3)
    expression (,loc ,chan ,amt3) wheel (,loc ,chan ,amt3)
    after (,loc ,chan ,amt3) control (,loc ,chan ,val4)
    program (,loc ,chan ,val3) tempo (,loc ,amt2) meta (,loc)))
  "The fields for each type as an x4 list: (name index parser formatter)")

;; These are used internally by the mvt-field-change command
(defconst !mvt-field-change-plist
  `(set ,#'!mvt-setval add ,#'!mvt-addval scale ,#'!mvt-scaleval 
    max ,#'!mvt-maxval min ,#'!mvt-minval quantize ,#'!mvt-quantval)
  "Mapping from user-entered field change operations to functions")
(defconst !mvt-field-value-plist
  `(random ,#'!mvt-random  interval ,#'!mvt-interval 
    linear ,#'!mvt-linear curve1 ,#'!mvt-curve1 curve2 ,#'!mvt-curve2)
  "Mapping from user-entered field value operations to functions")

;; the keymap information used by MVT
(defconst !mvt-cmd-map (make-sparse-keymap) "Keymap selected by C-c")
(defconst !mvt-vis-map (make-sparse-keymap)  "Keymap selected by C-c v")
(defconst !mvt-cmd-alist
  `(("e" . mvt-edit) ("i" . mvt-insert) ("p" . mvt-play) ("m" . mvt-mute) 
	("z" . mvt-undo) ("." . mvt-send) ("TAB" . mvt-record) ("n" . mvt-new) 
	("f" . mvt-field-change) ("+" . mvt-bump) ("r" . mvt-select-rule) 
	("l" . mvt-legato) ("RET" . mvt-stop) ("c" . mvt-copy) ("x" . mvt-cut) 
	("a" . mvt-select-all) 	("h" . mvt-select-here) ("s" . mvt-solo) 
	("d" . mvt-select-dechord) 	("t" . mvt-select-thin) ("j" . mvt-spurt)
	("<right>" . mvt-next) 	("<left>" . mvt-prev) ("v" . ,!mvt-vis-map))
  "Alist of keys and MVT commands")
(defconst !mvt-vis-alist
  `(("*" . all) ("n" . note) ("p" . program) ("s" . sustain) ("v" . volume)
	("e" . expression) ("w" . wheel) ("t" . tempo) ("m" . meta) ("b" . bend) 
	("c" . control) ("a" . after))
  "Alist of keys and types for visibility")

;; These are changed by mvt-play, mvt-record, mvt-stop, and mvt-spurt
(defvar !mvt-recording-p nil "Are we recording?")
(defvar !mvt-playing-p nil  "Are we playing?")
(defvar !mvt-spurt-proc nil "The mvt-spurt process or nil")

;; These are changed by the select and modify commands 
(defvar !mvt-selected nil "The lines selected in a buffer (as bol points)")
(make-variable-buffer-local '!mvt-selected)  
(defvar !mvt-clipboard nil "The events copied or cut as strings")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  the MVT mode defined

;; the major mode for editing mvt files
(define-derived-mode mvt-mode
  fundamental-mode "MVT" "Major mode for editing MVT files"
  (when (= 1 (point-max))                           ; add to an empty file
    (insert "start-file 4 4\nend-file\n") (goto-char 1))
  (save-excursion                                   ; get time signature info
    (while (looking-at "^ *;") (forward-line))  
    (unless (looking-at "^start-file *\\([0-9]+\\) *\\([0-9]+\\)") 
      (error "No start-file line!"))
    (setq !mvt-numer (string-to-number (match-string 1)))
    (setq !mvt-denom (string-to-number (match-string 2)))
    (setq !mvt-ubar (* !mvt-ubeat !mvt-numer)))
  (mvt-scan-buffer)                                 ; mark for visibility
  (setq global-disable-point-adjustment t)          ; fix bug with Emacs move
  (setq buffer-invisibility-spec (append !mvt-all-types nil))
  (mapc #'mvt-visibility mvt-file-ini-vis)          ; initialize visibility
  (when mvt-auto-start-spurt-p (mvt-spurt))         ; maybe start mvt-spurt
  (define-key (current-local-map) (kbd "C-c") !mvt-cmd-map)
  (suppress-keymap !mvt-cmd-map)                    ; keys for mvt commands
  (mapc (lambda (p) (eval `(define-key !mvt-cmd-map (kbd ,(car p)) ',(cdr p))))
		!mvt-cmd-alist)
  (suppress-keymap !mvt-vis-map)                    ; keys for visibility
  (mapc (lambda (p) (!mvt-toggle-show (car p) (cdr p))) !mvt-vis-alist)
  (message "This is MVT version %s" mvt-version))

(defun !mvt-toggle-show (str type)
  (eval `(define-key !mvt-vis-map (kbd ,str) 
		   (lambda (&optional alonep)
			 (interactive "P") 
			 (mvt-visibility ',type alonep) (redraw-display)))))

;; this minor mode removes the requirement of typing C-c before commands
(define-minor-mode mvt-prefix-mode "MVT minor mode with no C-c needed for keys"
  nil " Prefix" !mvt-cmd-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  visibility of lines in the file

(defun mvt-visibility (type &optional alonep)
  "Make events of TYPE visible or invisible. (TYPE can also be all)
   If ALONEP is not NIL, then the other events are made invisible."
  (if (eq type 'all) 
      (if buffer-invisibility-spec (setq buffer-invisibility-spec nil)
        (setq buffer-invisibility-spec (append !mvt-all-types nil)))
    (let ((invis-p (memq type buffer-invisibility-spec)))
      (when alonep (setq buffer-invisibility-spec (append !mvt-all-types nil)))
      (if invis-p (remove-from-invisibility-spec type)
        (add-to-invisibility-spec type)))))

(defun mvt-scan-buffer () 
  "Mark all the lines in the buffer according to its type of MIDI event."
  (interactive) 
  (!mvt-scan (point-min) (point-max)))

(defun !mvt-scan (start end)
  "Add visibility text property for all the lines between START and END pts"
  (let ((mod (buffer-modified-p)))
    (mvt-map-lines start end
        #'(lambda (pt type) 
            (put-text-property pt (!mvt-past-eol) 'invisible type)))
    (set-buffer-modified-p mod)))

(defun !mvt-past-eol () (1+ (point-at-eol)))

(defun mvt-map-lines (start end fn)
  "Apply FN to all visible lines between START and END pts.
   The function FN takes two arguments: the bol point and the type of event."
  (save-excursion
    (goto-char start)
    (while (< (setq start (point)) end)
      (when (not (invisible-p start)) 
        (let ((type (!mvt-this-type))) (when type (funcall fn start type))))
      (forward-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Move around in a buffer

(defun mvt-next (&optional bigp)
  "Go to the next track in a file buffer or note in a track buffer.
   With a prefix argument, go to the last track or note event."
  (interactive "P")
  (let ((pat (if (mvt-track-buffer-p) "note" "start-track")))
    (when (looking-at (concat ".*" pat)) (forward-line))
    (if (not bigp) (search-forward pat)
      (goto-char (point-max)) (search-backward pat))
    (goto-char (point-at-bol))
    (mvt-send)))
    
(defun mvt-prev (&optional bigp)
  "Go to the previous track in a file buffer or note in a track buffer.
   With a prefix argument, go to the first track or note event."
  (interactive "P")
  (let ((pat (if (mvt-track-buffer-p) "note" "start-track")))
    (if (not bigp) (search-backward pat)
      (goto-char (point-min)) (search-forward pat))
    (goto-char (point-at-bol))
    (mvt-send)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Solo and mute tracks

(defun mvt-solo (&optional othersp) 
  "Toggle whether the current track is in solo mode.
   With a prefix argument, ensure that all other tracks are not in solo mode."
  (interactive "P") 
  (save-excursion (!mvt-mark-track " solo" othersp))
  (save-excursion 
    (when (mvt-track-buffer-p) (set-buffer (buffer-base-buffer)))
    (let ((soloing-p nil))
      (mvt-map-tracks 
        #'(lambda ()
            (goto-char (point-min))
            (search-forward "\"" (!mvt-past-eol) t 2) ; skip over track name
            (when (search-forward " solo" (!mvt-past-eol) t)
              (setq soloing-p t))))
      (goto-char (point-min))
      (if (search-forward " soloing" (!mvt-past-eol) t)
        (when (not soloing-p) (replace-match ""))
        (when soloing-p (goto-char (point-at-eol)) (insert " soloing"))))))

(defun mvt-mute (&optional othersp) 
  "Toggle whether the current track is in mute mode.
   With a prefix argument, ensure that all other tracks are not in mute mode."
  (interactive "P") 
  (save-excursion (!mvt-mark-track " mute" othersp)))

(defun !mvt-mark-track (str othersp)
  (when (mvt-track-buffer-p) (goto-char (point-min)))
  (let ((name (!mvt-this-track)))
    (if (search-forward str (!mvt-past-eol) t) (replace-match "")
      (goto-char (point-at-eol))
      (insert str))
    (when othersp
      (when (mvt-track-buffer-p) (set-buffer (buffer-base-buffer)))
      (mvt-map-tracks 
       #'(lambda () 
           (unless (string-equal (buffer-name) name)
             (goto-char (point-min))
             (when (search-forward str (!mvt-past-eol) t)
               (replace-match ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Operations for use with mvt-spurt for realtime MIDI

(defun mvt-spurt (&optional get-device-p)
  "Start or stop the Racket mvt-spurt.scm program to interact with MIDI.
   The mvt-input-device and mvt-output-device are used unless a prefix
   argument is provided, in which case they are prompted for."
  (interactive "P")
  (if !mvt-spurt-proc 
      (progn (mvt-spurt-cmd "(close-midi-devices)")
             (kill-process !mvt-spurt-proc) (setq !mvt-spurt-proc nil))
    (setq !mvt-spurt-proc 
       (start-process "myracket" "*racket*" mvt-racket "-it" "mvt-spurt.scm"))
    (set-process-query-on-exit-flag !mvt-spurt-proc nil)
    (when get-device-p 
      (mvt-spurt-cmd "(display-midi-devices)")
      (display-buffer "*racket*")
      (setq mvt-input-device
        (read-from-minibuffer "Input device (nil for none): " "nil" nil t))
      (setq mvt-output-device
        (read-from-minibuffer "Output device (nil for none): " "nil" nil t)))
    (mvt-spurt-cmd
       (format "(open-midi-devices %s %s)" 
          (or mvt-input-device "#f")
          (or mvt-output-device "#f")))))

(defun mvt-spurt-cmd (str) 
  "Ask mvt-spurt to execute the command given by the string STR."
  (if (not !mvt-spurt-proc) (error "No mvt-spurt process running")
    (process-send-string !mvt-spurt-proc (concat str "\n"))))

(defun mvt-send ()
  "When editing a track, send the current line to MIDI out."
  (interactive)
  (when (and !mvt-spurt-proc (mvt-track-buffer-p))
    (process-send-string !mvt-spurt-proc 
        (concat "(play-event \"" 
                (buffer-substring-no-properties (point) (point-at-eol))
                "\")\n"))))

(defun mvt-tap-tempo ()
  "Change the mvt-tempo-bpm by tapping keys, until a RETURN is tapped."
  (interactive)
  (let ((d3 .5) (d2 .5) (d1 .5) (st (float-time)) (end 0) (bpm 120.0))
    (while 
        (/= 13 (read-char (format "Tap tempo (exit with RETURN): %.0f" bpm)))
      (setq end (float-time)) (setq d3 d2) (setq d2 d1) (setq d1 (- end st))
      (setq bpm (fround (/ 180.0 (+ d1 d2 d3))))
      (setq st end))
    (message "Tempo is set to %s" bpm)
    (setq mvt-tempo-bpm bpm)))

(defun mvt-record (&optional reroute)
  "Play the current MIDI file (with mvt-play) and start recording from MIDI in.
  The mvt-stop command stops the recording and does the actual insertion of 
  events into a new track.  A prefix argument changes the MIDI channel used 
  for recording.  There are also global variables that affect the recording:
  mvt-lead-in is the number of lead-in bars, mvt-tempo is the  default tempo, 
  mvt-clicking-p is whether or not a metronome should sound on the beat."
  (interactive "P")
  (unless (or (mvt-track-buffer-p) 
              (looking-at "start-track") (looking-at "end-file"))
    (error "Cannot create a track here"))
  (unless reroute (setq reroute 0))
  (mvt-spurt-cmd (format "(set-tempo %s)" mvt-tempo-bpm))
  (mvt-spurt-cmd (format "(start-recording %s)" reroute))
  (setq !mvt-recording-p t)
  (mvt-play mvt-lead-in)
  (message "Recording... "))

(defun mvt-play (&optional start) 
  "Play the current file as a standard MIDI file.
  In a file buffer, a prefix argument makes the file play from that bar.
  In a track buffer, the file plays from the current bar, but a prefix 
  arguments backs up the playing by that many bars.
  The variable mvt-tracking-p determines if the cursor  moves during play."
  (interactive "P")
  (when (mvt-track-buffer-p)
    (let ((here (!mvt-this-bar)))
      (setq start (if start (- here start) here))))
  (unless start (setq start 1))
  (when (< start 1) (setq start 1))
  (let ((mvtfile "/tmp/file.mvt"))
    (save-excursion
      (when (mvt-track-buffer-p) (set-buffer (buffer-base-buffer)))
      (write-region nil nil mvtfile))
    (when (and mvt-tracking-p (mvt-track-buffer-p))
      (set-process-filter !mvt-spurt-proc '!mvt-goto-filter)
      (mvt-spurt-cmd (concat "(start-tracking \"" (buffer-name) "\")")))
    (mvt-spurt-cmd (format "(schedule-file \"%s\" %s)" mvtfile start))
    (when (and !mvt-recording-p mvt-clicking-p) 
      (mvt-spurt-cmd "(start-clicking)"))
    (setq !mvt-playing-p t)
    (message "Playing... ")
    (mvt-spurt-cmd "(start-playing)") ))

(defun !mvt-goto-filter (proc str)
  "This is the procedure that is used as a filter for the MVT-SPURT process.
   It advances the cursor to the location given by STR."
  (when (string-match "^[0-9]+" str)
    (re-search-forward (concat "^ *" (substring str 0 -2) " ") nil t)
    (goto-char (point-at-bol))))

(defun mvt-stop ()
  "Stop any playing or recording that is in progress.
   For a recording, the new events are inserted into a new track."
  (interactive)
  (when !mvt-playing-p
    (mvt-spurt-cmd "(stop-playing)")
    (setq !mvt-playing-p nil)
    (mvt-spurt-cmd "(stop-tracking)")
    (set-process-filter !mvt-spurt-proc nil)
    (message "Playing... done"))
  (when !mvt-recording-p 
    (mvt-spurt-cmd "(stop-recording)")
    (setq !mvt-recording-p nil)
    (mvt-spurt-cmd "(stop-clicking)")
    (mvt-new 1 (symbol-name (gensym "Record")))
    (mvt-edit)
    (!mvt-insert-recorded-track)
    (message "Recording... done")))

(defun !mvt-insert-recorded-track ()
  "Convert the track in /tmp/track.csv to /tmp/track.mvt and insert it"
  (shell-command-to-string 
   (format "./rec-csv2mvt.pl %s %s" !mvt-numer !mvt-denom))
  (let ((start (point)) 
        (len (cadr (insert-file-contents "/tmp/track.mvt"))))
    (mvt-unselect-all)
    (!mvt-scan start (+ start len))
    (!mvt-select-lines start (+ start len) nil)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  creation of new tracks, MIDI events and buffers for tracks

(defun mvt-new (num what)
  "Create a track in a file buffer or MIDI events in a track buffer.
   In a file buffer, the track name is prompted for.
   In a track buffer, the MIDI event type is prompted for and an event of that
   type is added at the current location.
   With a prefix argument, that many events are added."
  (interactive 
     (list current-prefix-arg
        (if (mvt-track-buffer-p) 
            (read-minibuffer "Type of event (note, bend, etc): ")
          (read-from-minibuffer "Name of track (without quotes): "))))
  (if (mvt-track-buffer-p) 
      (!mvt-merge (mvt-create-default-events what num (!mvt-this-bar)))
    (unless (stringp what) (error "Track name is not a string"))
    (unless (or (looking-at "start-track") (looking-at "end-file"))
      (error "Cannot create a track here"))
    (save-excursion (insert "start-track \"" what "\"\nend-track\n"))))

(defun mvt-edit ()
  "Go back and forth between editing a file buffer and a track buffer."
  (interactive)
  (switch-to-buffer (or (buffer-base-buffer) (!mvt-this-track-buffer))))

(defun mvt-track-buffer-p () "Are we editing a track?" (buffer-base-buffer))

(defun mvt-map-tracks (fn)
  "Apply thunk FN for each track buffer (creating them as necessary)"
  (goto-char (point-min))
  (while (search-forward "start-track" nil t)
    (beginning-of-line)
    (with-current-buffer (!mvt-this-track-buffer) (funcall fn))
    (forward-line)))

(defun !mvt-this-track-buffer ()
  "The name of the (possibly new) buffer for the track at the current cursor"
  (let ((start (point)) (end nil) (name (!mvt-this-track)))
    (unless (get-buffer name)
      (save-excursion 
        (if (search-forward "end-track" nil t) (setq end (!mvt-past-eol))
          (error "Cannot find end of track %s" name))
        (set-buffer (make-indirect-buffer (current-buffer) name t))
        (narrow-to-region start end)
        (setq buffer-invisibility-spec (append !mvt-all-types nil))
        (mapc #'mvt-visibility mvt-track-ini-vis)
        (goto-char (point-min))
        (forward-line)))
    name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Using the ClipBoard

(defun mvt-copy ()
  "The selected events are unselected and copied to the clipboard.
   The file is not modified."
  (interactive)
  (save-excursion 
    (setq !mvt-clipboard 
      (mapcar #'(lambda (ev) (goto-char ev) (!mvt-this-line-full)) 
              !mvt-selected))
    (mvt-unselect-all)))

(defun mvt-cut ()
  "The selected events are killed and placed on the clipboard.
   The file is modified."
  (interactive)
  (let ((tmp (reverse !mvt-selected)))
    (mvt-copy)
    (save-excursion 
      (mapc #'(lambda (ev) (goto-char ev) (delete-region ev (!mvt-past-eol)))
            tmp))))

(defun mvt-undo ()
  "Swap the clipboard and the selected events."
  (interactive)
  (let ((tmp !mvt-clipboard)) (mvt-cut) (!mvt-merge tmp)))

(defun mvt-insert (&optional herep) 
  "Merge the clipboard events into the current track.
   With a prefix argument, the events start at the current location."
  (interactive "P")
  (let ((offset (when herep (- (* !mvt-ubar (1- (!mvt-this-bar)))
                               (!mvt-get-units (car !mvt-clipboard) 1)))))
    (!mvt-merge !mvt-clipboard)
    (when offset 
      (!mvt-modify-selection #'!mvt-addval 'location (!mvt-constant offset)))))

(defun !mvt-merge (strs &optional modifyfn)
  "Merge the events in the STRS list onto the current track.
f   If MODIFYN is not NIL, first change the events according to MODIFYFN.
   The MODIYFN takes three arguments: i, n, list of event strings."
  (unless (mvt-track-buffer-p) (mvt-edit))
  (let ((start nil) (end nil) (str nil) (i 0) (n (- (length strs) 1))
        (ustr nil) (uprev 0) (line nil))
    (goto-char (point-min))
    (while strs 
      (setq str (if modifyfn (funcall modifyfn i n strs) (car strs)))
      (setq i (+ i 1))
      (setq strs (cdr strs))
      (setq ustr (!mvt-get-units str 1))
      (when (< ustr uprev) (goto-char (point-min))) ; watch for misplaced event
      (while (and (not (looking-at "end-track"))
                  (or (not (!mvt-this-type (setq line (!mvt-this-line))))
                      (<= (!mvt-get-units line 1) ustr)))
        (forward-line))
      (setq uprev ustr) (setq start (point)) (insert str) (setq end (point))
      (goto-char start) (!mvt-select-line) (goto-char end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Modify the field values of selected events (cut then insert modified)

(defun mvt-field-change (&optional strength)
  "Modify the selected events in a track by changing a field.
   The type of change, field, and type of value are prompted for.
   (See the doc/ file for further details.)"
  (interactive "P")
  (let ((tmp1 nil) (tmp2 nil) (changefn nil) (field nil) (valfn nil))
    (setq tmp1 
          (read-from-minibuffer "Type of change (set, scale, etc): " "" nil t))
    (setq changefn (plist-get !mvt-field-change-plist tmp1))
    (unless changefn (error "Unrecognized type of field change: %s" tmp1))
    (setq tmp1 (read-from-minibuffer "Field to change: " "" nil t))
    (unless (memq tmp1 !mvt-all-fields) 
      (error "Field %s is not a member of %s" tmp1 !mvt-all-fields))
    (setq field tmp1)
    (setq tmp1 (read-from-minibuffer "New value for field: "))
    (setq tmp2 (!mvt-fix-string tmp1))
    (unless 
       (or (numberp tmp2) 
           (and (consp tmp2) (= 3 (length tmp2))
                (numberp (cadr tmp2)) (numberp (caddr tmp2))
                (memq (car tmp2) '(random interval linear curve1 curve2))))
      (error "Incorrect expression for new value: %s" tmp1))
    (setq valfn 
      (if (numberp tmp2) (!mvt-constant tmp2) 
        (apply (plist-get !mvt-field-value-plist (car tmp2)) (cdr tmp2))))
    (!mvt-modify-selection changefn field valfn strength)))

(defun mvt-bump (&optional downp)
  "Call mvt-field-change with parameters: scale, amount, 110.
   With a prefix argument, this becomes: scale, amount, 90."
  (interactive "P")
  (!mvt-modify-selection 
     #'!mvt-scaleval 'amount (!mvt-constant (if downp 90 110))))

(defun !mvt-modify-selection (changefn field valfn &optional strength)
  "Apply CHANGEFN to VALFN of FIELD value for each event in selection.
   This cuts the selection to the clipboard and then inserts modified events.
   The oprional STRENGTH argument changes the strenth of the modification."
  (unless strength (setq strength 100))
  (mvt-cut)
  (!mvt-merge !mvt-clipboard 
    #'(lambda (i n evs)
        (let* ((ev (car evs)) (type (!mvt-this-type ev)) 
               (x4 (assq field (plist-get !mvt-field-plist type))))
          (if (null x4) ev
            (let* ((fields (!mvt-fields ev))
                   (old (!mvt-get-field x4 fields))
                   (new (funcall changefn old (funcall valfn i n))))
              (when (< strength 100)
                (setq new (+ old (/ (* strength (- new old)) 100))))
              (!mvt-set-field x4 fields new)
              (mvt-assemble-new-event fields type)))))))

(defun mvt-legato (&optional strength)
  "Change the duration of selected notes to last until the next note.
   A prefix argument changes the strength of the legato."
  (interactive "P")
  (unless strength (setq strength 100))
  (mvt-cut)
  (!mvt-merge !mvt-clipboard 
    #'(lambda (i n evs)
        (let* ((ev (car evs)) (rest (cdr evs)) 
               (locx4 (assq 'location (plist-get !mvt-field-plist 'note)))
               (durx4 (assq 'duration (plist-get !mvt-field-plist 'note))))
          (if (not (eq 'note (!mvt-this-type ev))) ev
            (while (and rest (not (eq 'note (!mvt-this-type (car rest)))))
              (setq rest (cdr rest)))
            (if (null rest) ev
              (let* ((fields (!mvt-fields ev))
                     (curloc (!mvt-get-field locx4 fields))
                     (nxtloc (!mvt-get-field locx4 (!mvt-fields (car rest)))))
                (!mvt-set-field durx4 fields 
                                 (/ (* (- nxtloc curloc) strength) 100))
                (mvt-assemble-new-event fields 'note))))))))

;; the change functions
(defun !mvt-setval (old v) v)
(defun !mvt-addval (old v) (+ old v))
(defun !mvt-scaleval (old v) (/ (* old v) 100))
(defun !mvt-maxval (old v) (min old v))
(defun !mvt-minval (old v) (max old v))
(defun !mvt-quantval (old v) (* v (/ (+ old (/ v 2)) v)))

;; the value functions
(defun !mvt-constant (val) 
  (lexical-let ((v val)) #'(lambda (i n) v)))
(defun !mvt-random (st end)
  (lexical-let ((lo (if (< st end) st end)) 
                (diff1 (+ 1 (if (< st end) (- end st) (- st end)))))
    #'(lambda (i n) (+ lo (random diff1)))))
(defun !mvt-linear (start end)
  (lexical-let ((n0 start) (diff (- end start)))
    #'(lambda (i n) (+ n0 (/ (* diff i) n)))))
(defun !mvt-interval (start int)
  (lexical-let ((n0 start) (diff int))
    #'(lambda (i n) (+ n0 (* diff i)))))
(defun !mvt-curve1 (start end)
  (lexical-let ((n0 start) (diff (- end start)))
    #'(lambda (i n) (+ n0 (/ (* diff i i) (* n n))))))
(defun !mvt-curve2 (start end)
  (lexical-let ((n0 end) (diff (- start end)))
    #'(lambda (i n) (+ n0 (/ (* diff (- n i) (- n i)) (* n n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Selecting events for manipulation

(defun !mvt-select-line (&optional removep)
  "Add/remove the current line to the selection"
  (let ((pt (point)) (end (!mvt-past-eol)) (mod (buffer-modified-p)))
    (if removep (remove-text-properties pt end '(face highlight))
      (add-text-properties pt end '(face highlight)))
    (set-buffer-modified-p mod)
    (setq !mvt-selected (if removep (delete pt !mvt-selected) 
                        (gnus-add-to-sorted-list !mvt-selected pt)))))

(defun !mvt-select-lines (start end removep &optional rulefn)
  "Add/remove to selection all visible lines between START and END.
   If RULEFN is not nil, the lines must also pass RULEFN." 
  (mvt-map-lines start end
    #'(lambda (pt type)
        (when (or (null rulefn) (funcall rulefn (!mvt-this-line)))
          (!mvt-select-line removep)))))

(defun mvt-unselect-all ()
  "Unselect each element of the selection"
  (let ((mod (buffer-modified-p)))
    (mapc #'(lambda (ev) 
              (goto-char ev)
              (remove-text-properties ev (!mvt-past-eol) '(face highlight)))
          !mvt-selected)
    (set-buffer-modified-p mod)
    (setq !mvt-selected nil)))

(defun !mvt-sel-thin (flip n selects)
  "If FLIP it t, deselect every Nth element from list SELECTS.
   If FLIP it nil, deselect all but the Nth elements"
  (let ((i n))
    (while selects
      (when (not flip) (goto-char (car selects)) (!mvt-select-line t))
      (setq i (1- n)) (setq selects (cdr selects))
      (while (and selects (> i 0))
        (when flip (goto-char (car selects)) (!mvt-select-line t))
        (setq i (1- i)) (setq selects (cdr selects))))))

(defun !mvt-sel-rule (rule &optional removep)
  "Add/remove all lines in buffer that pass RULE."
  (!mvt-select-lines (point-min) (point-max) removep
    `(lambda (ev) 
       (let* ((fields (!mvt-fields ev)) (type (!mvt-this-type ev))
              (ves (mapcar 
                    #'(lambda (x4) (list (car x4) (!mvt-get-field x4 fields)))
                    (plist-get !mvt-field-plist type))))
         (eval `(let ((type ',type) (beats ,!mvt-ubeat) 
                      (bars ,!mvt-ubar) ,@ves) 
                  (let ((bar-distance (!mvt-distance location !mvt-ubar))
                        (beat-distance (!mvt-distance location !mvt-ubeat)))
                    (ignore-errors ,rule))))))))

;; add/remove notes if they are near the top/bot of notes sounding at once
(defun !mvt-sel-dechord (num topp removep)
  (let ((trips nil)
        (locx4 (assq 'location (plist-get !mvt-field-plist 'note)))
        (pitx4 (assq 'pitch (plist-get !mvt-field-plist 'note))))
    (mvt-map-lines (point-min) (point-max)
      #'(lambda (pt type)            ; get all notes in form (pt loc pitch)
          (when (eq 'note type)
            (let* ((fields (!mvt-fields (!mvt-this-line)))
                   (loc (!mvt-get-field locx4 fields))
                   (pitch (!mvt-get-field pitx4 fields)))
              (setq trips  (cons (list pt loc pitch) trips))))))
    (setq trips (reverse trips))     ; put note triples into chrono order
    (let ((k 0) (x1 trips) (z trips) (x2 trips))
      (while x1                      ; go through all the triples
        (while (< (cadr (car z)) (- (cadr (car x1)) 32)) (setq z (cdr z)))
        (setq k 0) (setq x2 z)       ; look for close notes
        (while (and x2 (< (cadr (car x2)) (+ (cadr (car x1)) 32)))
          (when (or (and topp (> (caddr (car x2)) (caddr (car x1))))
                    (and (not topp) (< (caddr (car x2)) (caddr (car x1)))))
            (setq k (1+ k)))         ;  count close notes with higher pitch
          (setq x2 (cdr x2)))
        (when (< k num)              ; if k is small, add note to selection
          (goto-char (car (car x1)))
          (!mvt-select-line removep))
        (setq x1 (cdr x1))))))

;; the selection commands: parse and check args and then call functions above

(defun mvt-select-all (&optional removep) 
  "Add/remove all the visible events in the track to the selection."
  (interactive "P") 
  (unless (mvt-track-buffer-p) 
    (error "Can only select/unselect events in a track buffer"))
  (!mvt-select-lines (point-min) (point-max) removep))

(defun mvt-select-here (&optional removep) 
  "Add/remove the current line or region to the selection."
  (interactive "P") 
  (unless (mvt-track-buffer-p) 
    (error "Can only select/unselect events in a track buffer"))
  (if (use-region-p)
      (!mvt-select-lines (region-beginning) (region-end) removep)
    (!mvt-select-line removep)))
  
(defun mvt-select-thin (&optional flip)
  "Remove every Nth event from the selection starting at event M.
   The N and M are prompted for.  With a prefix argument, those events
   remain in the selection and the others are removed instead."
  (interactive "P")
  (unless (mvt-track-buffer-p) 
    (error "Can only select/unselect events in a track buffer"))
  (save-excursion 
    (!mvt-sel-thin flip
       (read-from-minibuffer "Every Nth event for N: " "2" nil t)
       (append 
         (nthcdr (1- (read-from-minibuffer "Starting from event: " "1" nil t))
                 !mvt-selected)
         nil))))

(defun mvt-select-rule (&optional removep)
  "Add/remove to the selection those events that pass a certain rule.
   The rule is prompted for.  (See the doc/ file for details.)"
  (unless (mvt-track-buffer-p) 
    (error "Can only select/unselect events in a track buffer"))
  (interactive "P")
  (!mvt-sel-rule (!mvt-fix-string (read-from-minibuffer "Rule: ")) removep))
    
(defun mvt-select-dechord (&optional removep)
  "Add/remove notes to the selection but with limited polyphony.
   The number of simultaneous notes and whether to add the high or low ones 
   to the selection is prompted for."
  (interactive "P")
  (unless (mvt-track-buffer-p) 
    (error "Can only select/unselect events in a track buffer"))
  (save-excursion 
    (!mvt-sel-dechord 
      (read-from-minibuffer "Max number of simultaneous notes: " "1" nil t)
      (read-from-minibuffer "Apply to highest notes? (t or nil): " "t" nil t)
      removep)))

;; some utility functions

(defun !mvt-distance (loc grid)
  "The distance of location LOC to a grid point GRID."
  (let ((mod (% loc grid))) (if (<= mod (/ grid 2)) mod (- grid mod))))

(defun !mvt-fix-string (str)
  "Replace locations and note names (in quotes) within STR by numbers" 
  (while (string-match "\"\\([0123456789:]*\\)\"" str)     ; locations
    (let ((repl (save-match-data (!mvt-parse-location (match-string 1 str)))))
      (setq str (replace-match (number-to-string repl) nil nil str))))
  (while (string-match "\"\\([^\"]*\\)\"" str)             ; note names 
    (let ((repl (save-match-data (!mvt-parse-pitch (match-string 1 str)))))
      (setq str (replace-match (number-to-string repl) nil nil str))))
  (car (read-from-string str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Create or destroy time by bar (using select and modify commands)

(defun !mvt-add-time (start dur)
  "Add time in a single track."
  (mvt-unselect-all)
  (!mvt-sel-rule `(>= location ,start))
  (!mvt-modify-selection #'!mvt-addval 'location (!mvt-constant dur))
  (mvt-unselect-all))

(defun !mvt-kill-time (start dur)
  "Kill time in a single track."
  (mvt-unselect-all)
  (!mvt-sel-rule `(and (>= location ,start) (< location ,(+ start dur))))
  (mvt-cut)
  (!mvt-sel-rule `(>= location ,(+ start dur)))
  (!mvt-modify-selection #'!mvt-addval 'location (!mvt-constant (- dur)))
  (mvt-unselect-all))

(defun mvt-shift-bars (killp startbar durbar)
  "Insert blank time by moving all events in the track forward.
   The start bar and the number of bars to add are prompted for.
   In a file buffer, do this to every track.
   With a prefix argument, delete events and move events backward."
  (interactive 
    (list current-prefix-arg 
      (read-minibuffer "Start at bar: ")
      (if current-prefix-arg (read-minibuffer "Bars to kill: ")
        (read-minibuffer "Bars to add: "))))
  (save-excursion 
    (let ((startu (* (- startbar 1) !mvt-ubar)) (duru (* durbar !mvt-ubar)))
      (if killp 
          (if (mvt-track-buffer-p) (!mvt-kill-time startu duru)
            (mvt-map-tracks #'(lambda () (!mvt-kill-time startu duru))))
        (if (mvt-track-buffer-p) (!mvt-add-time startu duru)
          (mvt-map-tracks #'(lambda () (!mvt-add-time startu duru))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Construct and deconstruct MIDI events by type and by field

;;  functions to use an X4 list (fieldname index parserfn formatterfn)

(defun !mvt-get-field (x4 fields) 
  "Use X4 to get a field in FIELDS."
  (funcall (caddr x4) (car (nthcdr (cadr x4) fields))))

(defun !mvt-set-field (x4 fields new) 
  "Use X4 to set a field in FIELDS to value NEW."
  (setcar (nthcdr (cadr x4) fields) (funcall (cadddr x4) new)))

;;  functions to retrieve date from the current line in a buffer

(defun !mvt-this-line () 
  "the current line in the buffer as a string without properties"
  (buffer-substring-no-properties (point) (!mvt-past-eol)))

(defun !mvt-this-line-full () 
  "the current line in the buffer as a string with properties"
  (buffer-substring (point) (!mvt-past-eol)))

(defun !mvt-fields (str)
  "parse the current line buffer into a list of string-valued fields"
  (let ((regx " *[^ \n]+") (start 0) (flds nil) (len (length str)))
    (string-match (concat regx " *") str start)
    (setq flds (list (substring str (match-beginning 0) (- (match-end 0) 1))))
    (setq start (match-end 0))
    (while (and (< start len) (string-match regx str start))
      (setq flds (cons (substring str (match-beginning 0) (match-end 0)) flds))
      (setq start (+ (match-end 0) 1)))
    (nreverse flds)))

(defun !mvt-this-type (&optional str)
  "the MIDI type of the current line in the buffer (or nil).
   If STR is not nil, then it is used instead of the current line."
  (unless str (setq str (!mvt-this-line)))
  (unless (or (string-match "^ *;" str) (string-match "start-file" str) 
              (string-match "start-track" str) (string-match "end-file" str)
              (string-match "end-track" str))
    ;; line is of the form loc type args
    (intern (cadr (!mvt-fields str)))))

(defun !mvt-this-track ()
  "the name of the track in the current \"start-track\" line in the buffer" 
  (unless (looking-at "start-track *\"\\(.*\\)\"")
    (error "Not at start of track"))
  (match-string 1))

(defun !mvt-this-bar ()
  "the bar of the current line in a track buffer"
  (unless (mvt-track-buffer-p) 
    (error "Trying to get a bar location outside a track buffer"))
  (let ((endp nil))
    (when (looking-at "end-track") (forward-line -1) (setq endp t))
    (if (looking-at "start-track") 1
      (unless (looking-at "^ *\\([^:]*\\):")
        (error "Can't find bar in a track buffer"))
      (+ (if endp 1 0) (string-to-number (match-string 1))))))

;;  functions to format and parse field values

(defun !mvt-format-location (u) (if (numberp u) (!mvt-get-bar-beat u 1) u))
(defun !mvt-format-channel (c) (format "%s" (max 1 (min 16 c))))
(defun !mvt-format-pitch (p) 
  (format "%3s" (if (numberp p) (!mvt-note-to-name p) p)))
(defun !mvt-format-value (v) (format "%s" (max 0 (min 127 v))))
(defun !mvt-format-velocity (v) (format "%3s" (max 0 (min 127 v))))
(defun !mvt-format-duration (u) (if (numberp u) (!mvt-get-bar-beat u 0) u))
(defun !mvt-format-bend (v) (number-to-string (max -8192 (min 8191 v))))
(defun !mvt-format-bpm (v) (format "%.3f" (max 0 v)))

(defun !mvt-parse-location (loc) (if (numberp loc) loc (!mvt-get-units loc 1)))
(defun !mvt-parse-duration (dur) (if (numberp dur) dur (!mvt-get-units dur 0)))
(defun !mvt-parse-pitch (p) (if (numberp p) p (!mvt-name-to-note p)))

(defun !mvt-get-units (str &optional base)
  "Convert x:y:z in STR to MIDI units.  When BASE=0 this is for durations."
  (unless base (setq base 0))
  (let ((bar-str "0") (beat-str "0") (unit-str "0"))
	(if (string-match "\\([^:]*\\):\\([^:]*\\):\\(.+\\)" str)
		(progn (setq bar-str (match-string 1 str))
			   (setq beat-str (match-string 2 str))
			   (setq unit-str (match-string 3 str)))
	  (if (equal base 1) (error "%s is not a valid location string" str)
		(if (string-match "\\([^:]*\\):\\(.+\\)" str)
			(progn (setq beat-str (match-string 1 str))
				   (setq unit-str (match-string 2 str)))
		  (setq unit-str str))))
	(let ((bar (string-to-number bar-str)) 
		  (beat (string-to-number beat-str))
		  (unit (string-to-number unit-str)))
	  (+ (* (- bar base) !mvt-ubar) (* (- beat base) !mvt-ubeat) unit))))

(defun !mvt-get-bar-beat (u &optional base)
  "Convert U in MIDI units to x:y:z.  When BASE=0 this is for durations."
  (unless base (setq base 0))
  (let* ((bar (/ u !mvt-ubar)) (beat (/ (% u !mvt-ubar) !mvt-ubeat))
         (unit (% (% u !mvt-ubar) !mvt-ubeat)))
    (if (equal base 0) 
		(if (= bar 0) 
			(if (= beat 0) (format "%s" unit) (format "%1s:%s" beat unit))
		  (format "%1s:%1s:%s" bar beat unit))
      (format "%3s:%1s:%-3s" (+ bar base) (+ beat base) unit))))

(defun !mvt-name-to-note (str)
  "Convert a pitch name STR to a number from 0-127"
  (let ((acc 0) (case-fold-search ()))
    (when (string-match "#" str) 
      (setq str (replace-match "" nil nil str)) 
      (setq acc 1))
    (when (string-match "b" str) 
      (setq str (replace-match "" nil nil str)) 
      (setq acc -1))
    (unless (string-match "\\([CDEFGAB]\\)\\([-0123456789]+\\)" str)
      (error (concat "The string " str " does not look like a note")))
    (+ (* 12 (+ 1 (string-to-number (match-string 2 str))))
       acc (cdr (assoc (match-string 1 str) !mvt-pitch-values)))))

(defun !mvt-note-to-name (p)
  "Convert a pitch number 0-127 to a note name"
  (let* ((oct (- (/ p 12) 1)) (sc (* 2 (% p 12)))
         (str (substring !mvt-pitch-string sc (+ sc 2))))
    (concat str (number-to-string oct))))

;;  functions to create new MIDI events

(defun mvt-create-default-events (type number bar)
  "Return a list of a NUMBER of default MIDI events of TYPE at BAR."
  (unless number (setq number 1))
  (unless bar (setq bar 1))
  (unless (memq type !mvt-all-types) 
    (error "Unrecognized event to create: %s" type))
  (let ((ev (plist-get !mvt-default-plist type))
        (location (!mvt-format-location (* (- bar 1) !mvt-ubar))))
    (make-list number (propertize (concat location " " ev "\n") 
                                  'invisible type))))

(defun mvt-assemble-new-event (fields type)
  "Create a new MIDI event of TYPE using the field values given in FIELDS."
  (propertize (concat (mapconcat #'(lambda (x) x) fields " ") "\n")
              'invisible type))
