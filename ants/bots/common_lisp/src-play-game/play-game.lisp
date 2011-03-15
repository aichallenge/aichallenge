;;;; play-game.lisp

(in-package :play-game)


;;; Variables

(defparameter *verbose* nil)

(defparameter *debug* *debug-io*)
(defparameter *input* *standard-input*)
(defparameter *output* *standard-output*)

(defparameter *bots* nil)
(defparameter *procs* nil)

(defparameter *map-cols* nil)
(defparameter *map-rows* nil)
(defparameter *map-players* nil)
(defparameter *map* nil)
(defparameter *map-array* nil)
(defparameter *turns* 200)
(defparameter *load-time* 3000)
(defparameter *turn-time* 1000)

(defparameter +version+ "0.1a")


;;; Common Functions

(defun logmsg (&rest args)
  (when *verbose*
    (let ((str (with-output-to-string (s)
                 (dolist (a args)
                   (princ a s)))))
      (format *debug* str)
      (force-output *debug*))))


;;; Handlers

(defun error-handler (&optional arg)
  (logmsg "~&" arg " Aborting...~%")
  (exit 1))


(defun interrupted-by-user (arg)
  (declare (ignore arg))
  (logmsg "~&Interrupted by user. Aborting...~%")
  (exit))


;;; Functions

(defun do-turn (turn)
  (loop for proc in *procs*
        for i from 0
        for bot = (elt *bots* i)
        for pin = (sb-ext:process-input proc)
        for pout = (sb-ext:process-output proc)
        do  (if (equal :running (sb-ext:process-status proc))
                (send-game-state i pin turn)
                (logmsg i ":" bot " has stopped running...~%"))
           (let ((turn-start (wall-time)))
             (wait-for-output pout turn-start)
             (loop ;while (wait-for-output pout start)  ; no return values
                   with end-loop = nil
                   until end-loop
                   do (cond ((no-turn-time-left-p turn-start)
                             (logmsg i ":" bot " took too long.~%")
                             (setf end-loop t))
                            ((listen pout)
                             (let ((line (read-line pout nil)))
                               (when (starts-with line "go")
                                 (loop-finish))
                               (logmsg i ":" bot ": " line "~%")
                               (when (starts-with line "o ")
                                 (move-ant i line)))))))))


(defun move-ant (botid line)
  (let* ((split (split-sequence #\space (string-upcase line)))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (dir (elt split 3)))
    (setf (aref *map-array* row col) 0)
    (cond ((equal dir "N")
           (setf (aref *map-array*
                       (if (= row 0) (- *map-rows* 1) (- row 1))
                       col)
                 (+ botid 100)))
          ((equal dir "E")
           (setf (aref *map-array*
                       row
                       (if (= col (- *map-cols* 1)) 0 (+ col 1)))
                 (+ botid 100)))
          ((equal dir "S")
           (setf (aref *map-array*
                       (if (= row (- *map-rows* 1)) 0 (+ row 1))
                       col)
                 (+ botid 100)))
          ((equal dir "W")
           (setf (aref *map-array*
                       row
                       (if (= col 0) (- *map-cols* 1) (- col 1)))
                 (+ botid 100))))))


(defun parse-map (map)
  (with-open-file (f map)
    (loop with rows = 0
          for line = (read-line f nil)
          while line
          do (cond ((starts-with line "cols ")
                    (setf *map-cols* (par-value line)))
                   ((starts-with line "rows ")
                    (setf *map-rows* (par-value line)))
                   ((starts-with line "players ")
                    (setf *map-players* (par-value line)))
                   ((and (starts-with line "m ") (null *map-cols*))
                    (logmsg "~&Map missing \"cols n\" line. Aborting...~%")
                    (exit))
                   ((and (starts-with line "m ") (null *map-rows*))
                    (logmsg "~&Map missing \"rows n\" line. Aborting...~%")
                    (exit))
                   ((and (starts-with line "m ") (null *map-players*))
                    (logmsg "~&Map missing \"players n\" line. Aborting...~%")
                    (exit))
                   ((and (starts-with line "m ")
                         (< (length *bots*) *map-players*))
                    (logmsg "~&Map needs " *map-players* " players but only "
                            (length *bots*) " were entered on the "
                            "command-line. Aborting...~%")
                    (exit))
                   ((and (starts-with line "m ") (null *map-array*))
                    (setf *map-array*
                          (make-array (list *map-rows* *map-cols*)
                                     :element-type 'fixnum :initial-element 0))
                    (parse-map-line *map-array* line rows)
                    (incf rows))
                   ((starts-with line "m ")
                    (parse-map-line *map-array* line rows)
                    (incf rows)))
             finally (when (/= rows *map-rows*)
                       (logmsg "~&Actual map rows (" rows ") not equal to "
                               "specified number of rows (" *map-rows* "). "
                               "Aborting...~%")
                       (exit)))))


(defun parse-map-line (map-array string row)
  (when (/= (- (length string) 2) *map-cols*)
    (logmsg "~&Actual map columns (" (- (length string) 2) ") for this line "
            "not equal to specified number of columns (" *map-cols* ") for "
            "this map. Aborting...~%")
    (exit))
  (loop for c across (subseq string 2)
        for col from 0
        do (setf (aref map-array row col)
                 (case c
                   (#\. 0)
                   (#\% 1)
                   (otherwise (+ (char-code c) 3))))))


(defun process-cmdline-options ()
  (cond ((or (getopt :short-name "?") (getopt :short-name "h")
             (= 1 (length (cmdline))))
         (help)
         (exit))
        ;; use :SAVE-RUNTIME-OPTIONS to SAVE-LISP-AND-DIE if you want --version
        ((getopt :long-name "release")
         (format t "~&play-game (Ant Wars) version ~A~%" +version+)
         (exit)))
  (do-cmdline-options (option name value source)
    (setf source source)  ; to silence compilation warnings  TODO remove!
    (cond ((or (equal name "m") (equal name "map_file"))
           (setf *map* value))
          ((or (equal name "r") (equal name "rounds"))
           (setf *turns* (parse-integer value)))
          ((or (equal name "t") (equal name "turns"))
           (setf *turns* (parse-integer value)))
          ((or (equal name "v") (equal name "verbose"))
           (setf *verbose* t))
          ((equal name "loadtime")
           (setf *load-time* (parse-integer value)))
          ((equal name "turntime")
           (setf *turn-time* (parse-integer value)))))
  (unless *map*
    (help)
    (exit))
  (setf *bots* (loop for bot in (remainder) collect bot)))


(defun run-program (program &optional (args nil))
  ;; If there's a space in PROGRAM assume it needs to be started with an
  ;; interpreter which needs a slightly different call to RUN-PROGRAM.
  (if (find #\space program)
      (let ((split (split-sequence #\space program)))
        (logmsg (format nil "Starting: ~S ~S~%" (car split) (cdr split)))
        (sb-ext:run-program (car split) (cdr split) :search t :wait nil
                            :input :stream :output :stream))
      (progn (logmsg (format nil "Starting: ~S~%" program))
             (sb-ext:run-program (merge-pathnames program) args :wait nil
                                 :input :stream :output :stream))))


(defun send-game-state (botid input turn)
  (format input "turn ~D~%" turn)
  (loop for row from 0 below *map-rows*
        do (loop for col from 0 below *map-cols*
                 for sq = (aref *map-array* row col)
                 do (cond ((= sq 1) (format input "w ~D ~D~%" row col))
                          ((= sq 2) (format input "f ~D ~D~%" row col))
                          ((>= sq 200)
                           (format input "d ~D ~D ~D~%" row col
                                   (cond ((= botid (- sq 200)) 0)
                                         ((< botid (- sq 200)) (- sq 200))
                                         ((> botid (- sq 200)) (- sq 199)))))
                          ((>= sq 100)
                           (format input "a ~D ~D ~D~%" row col
                                   (cond ((= botid (- sq 100)) 0)
                                         ((< botid (- sq 100)) (- sq 100))
                                         ((> botid (- sq 100)) (- sq 99))))))))
  (format input "go~%")
  (force-output input))


(defun send-initial-game-state ()
  (loop for proc in *procs*
        for i from 0
        for bot = (elt *bots* i)
        for pin = (sb-ext:process-input proc)
        for pout = (sb-ext:process-output proc)
        do (if (equal :running (sb-ext:process-status proc))
               (progn
                 (format pin "turn 0~%loadtime ~D~%turntime ~D~%rows ~D~%cols ~D~%turns ~D~%viewradius2 93~%attackradius2 6~%spawnradius2 6~%ready~%"
                         *load-time* *turn-time* *map-rows* *map-cols* *turns*)
                 (force-output pin))
               (logmsg i ":" bot " has stopped running...~%"))
           ;; TODO don't do this when bot has stopped running
           (let ((turn-start (wall-time)))
             (wait-for-output pout turn-start)
             (cond ((no-turn-time-left-p turn-start)
                    (logmsg i ":" bot " took too long.~%"))
                   ((listen pout)
                    (let ((line (read-line pout nil)))
                      (unless (starts-with line "go")
                        (logmsg i ":" bot " sent junk: " line "~%"))))))))


(defun no-turn-time-left-p (turn-start-time)
  (not (turn-time-left-p turn-start-time)))


(defun turn-time-left-p (turn-start-time)
  (<= (- (wall-time) turn-start-time)
      (/ *turn-time* 1000)))


(defun wait-for-output (output turn-start-time)
  (loop until (or (listen output)
                  (no-turn-time-left-p turn-start-time))))


;;; Main Program

(defsynopsis (:postfix "BOT1 BOT2 .. BOTn")
  (text :contents "Game engine for Ant Wars.
")
  (group (:header "Game options:")
    (path :short-name "m" :long-name "map_file" :argument-name "MAP"
          :type :file :description "Name of the map file. (required)")
    (stropt :short-name "r" :long-name "rounds" :argument-name "ROUNDS"
            :default-value "200"
            :description "Number of rounds to play. (synonym for --turns)")
    (stropt :short-name "t" :long-name "turns" :argument-name "TURNS"
            :default-value "200"
            :description "Number of turns in the game.")
    (path :short-name "o" :long-name "output_dir" :argument-name "OUTPUT_DIR"
          :type :directory :default-value #p"replays/"
          :description "Directory to dump replay files to. (defunct)")
    (flag :long-name "serial"
      :description "Run bots in serial, instead of parallel. (defunct)")
    (stropt :long-name "loadtime" :argument-name "LOADTIME"
            :default-value "3000"
            :description "Amount of time to give for load, in milliseconds.")
    (stropt :long-name "turntime" :argument-name "TURNTIME"
            :default-value "1000"
            :description "Amount of time to give each bot, in milliseconds.")
    (stropt :long-name "attack" :argument-name "ATTACK"
            :description "Attack method to use for engine. (ignored)"))
  (group (:header "Debug options:")
    (flag :short-name "v" :long-name "verbose"
      :description "Print out status as game goes.")
    (flag :short-name "I" :long-name "log_input"
      :description "Log input streams sent to bots. (defunct)")
    (flag :short-name "O" :long-name "log_output"
      :description "Log output streams from bots. (defunct)"))
  (group (:header "Immediate exit options:")
    (flag :short-name "?"
      :description "Print this help and exit.")
    (flag :short-name "h" :long-name "help"
      :description "Print this help and exit.")
    (flag :long-name "release"
	  :description "Print version/release number and exit.")))


(defun main ()
  (make-context)
  (process-cmdline-options)
  (parse-map *map*)
  (let ((cdts (current-date-time-string)))
    (logmsg "~&=== New Match: " cdts " ===~%")
    (logmsg "[start] " cdts "~%"))
  (handler-bind (#+sbcl (sb-sys:interactive-interrupt #'interrupted-by-user))
                 ;(error #'error-handler))
    (loop for bot in *bots* collect (run-program bot) into procs
          finally (setf *procs* procs))
    (loop for turn from 0 to *turns*
          do (logmsg "--- turn: " turn " ---~%")
             (when *verbose* (print-game-map *map-array* *debug*))
             (when (= turn 0)
               (send-initial-game-state))
             (when (> turn 0)
               (do-turn turn))))
  (logmsg "[  end] " (current-date-time-string) "~%"))
