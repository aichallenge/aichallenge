;;;; play-game.lisp
;;;;
;;;; This is really a hack job currently.

(in-package :play-game)


;;; Variables

(defparameter *verbose* nil)

(defparameter *debug* *debug-io*)
(defparameter *input* *standard-input*)
(defparameter *output* *standard-output*)

(defparameter *bots* nil)
(defparameter *procs* nil)

(defparameter *map-rows* nil)
(defparameter *map-cols* nil)
(defparameter *map-players* nil)
(defparameter *map* nil)
(defparameter *map-array* nil)
(defparameter *orders* nil)
(defparameter *turns* 200)
(defparameter *load-time* 3000)
(defparameter *turn-time* 1000)
(defparameter *attack-radius* (sqrt 6))
(defparameter *spawn-radius* (sqrt 6))

(defparameter +version+ "0.1a")


;;; Common Functions

;; This is for the handlers in src-common/handlers.lisp.
(defun errmsg (&rest args)
  (apply #'logmsg args))


(defun logmsg (&rest args)
  (when *verbose*
    (let ((str (with-output-to-string (s)
                 (dolist (a args)
                   (princ a s)))))
      (format *debug* str)
      (force-output *debug*))))


;;; Functions

(defun ants-within-attack-range ()
  (loop with all = (loop for row from 0 below *map-rows*
                         append (loop for col from 0 below *map-cols*
                                      for value = (aref *map-array* row col)
                                      when (and (>= value 100) (<= value 199))
                                        collect (list value row col)))
        for ant-a in all
        for aid = (elt ant-a 0)
        for arow = (elt ant-a 1)
        for acol = (elt ant-a 2)
        append (loop for ant-b in (remove ant-a all)
                     for bid = (elt ant-b 0)
                     for brow = (elt ant-b 1)
                     for bcol = (elt ant-b 2)
                     for dist = (distance arow acol brow bcol)
                     when (and (/= aid bid)
                               (<= dist *attack-radius*))
                       collect (list :a-row arow :a-col acol
                                     :b-row brow :b-col bcol
                                     :distance dist))
          into result
        finally (return (sort result #'< :key (lambda (plist)
                                                (getf plist :distance))))))


;; This implements battle resolution 2 as described here:
;; - https://github.com/aichallenge/aichallenge/wiki/Ants-Game-Specification
(defun battle-resolution2 ()
  (loop for awar = (ants-within-attack-range)
        for battle = (first awar)
        while battle
        for arow = (getf battle :a-row)
        for acol = (getf battle :a-col)
        for aid = (when battle (- (aref *map-array* arow acol) 99))
        for brow = (getf battle :b-row)
        for bcol = (getf battle :b-col)
        for bid = (when battle (- (aref *map-array* brow bcol) 99))
        do (logmsg "Ants " aid ":" arow ":" acol " and " bid ":" brow ":" bcol
                   " fought...~%")
           (setf (aref *map-array* arow acol) (+ aid 199)
                 (aref *map-array* brow bcol) (+ bid 199))))


(defun battle-resolution ()
  (battle-resolution2))


;; TODO fix inefficient algorithm
(defun check-collisions ()
  (loop for order-a in (copy-seq *orders*)
        for bot-a-id = (getf order-a :bot-id)
        for srow-a = (getf order-a :src-row)
        for scol-a = (getf order-a :src-col)
        for row-a = (getf order-a :dst-row)
        for col-a = (getf order-a :dst-col)
        do (loop for order-b in (remove order-a (copy-seq *orders*))
                 for bot-b-id = (getf order-b :bot-id)
                 for srow-b = (getf order-b :src-row)
                 for scol-b = (getf order-b :src-col)
                 for row-b = (getf order-b :dst-row)
                 for col-b = (getf order-b :dst-col)
                 do (when (and (= row-a row-b)
                               (= col-a col-b))
                      (logmsg "Ants " bot-a-id ":" srow-a ":" scol-a " and "
                              bot-b-id ":" srow-b ":" scol-b " collided. "
                              "Killing ants...~%")
                      (setf (aref *map-array* srow-a scol-a) 0
                            (aref *map-array* srow-b scol-b) 0
                            *orders* (remove order-b
                                             (remove order-a *orders*)))))))


(defun check-positions ()
  (loop for order in (copy-seq *orders*)
        for bot-id = (getf order :bot-id)
        for row = (getf order :src-row)
        for col = (getf order :src-col)
        do (when (/= (+ bot-id 100) (aref *map-array* row col))
             (logmsg "Bot " bot-id " issued an order for a position it "
                     "doesn't occupy. Ignoring...~%")
             (setf *orders* (remove order *orders*)))))


(defun check-water ()
  (loop for order in (copy-seq *orders*)
        for bot-id = (getf order :bot-id)
        for row = (getf order :src-row)
        for col = (getf order :src-col)
        for dir = (getf order :direction)
        do (when (water? row col dir)
             (logmsg "Bot " bot-id " ordered an ant into water. Ignoring...~%")
             (setf *orders* (remove order *orders*)))))


(defun clear-dead-ants ()
  (loop for row from 0 below *map-rows*
        do (loop for col from 0 below *map-cols*
                 do (when (>= (aref *map-array* row col) 200)
                      (setf (aref *map-array* row col) 0)))))


(defun dir2key (direction)
  (cond ((equal direction "N") :north)
        ((equal direction "E") :east)
        ((equal direction "S") :south)
        ((equal direction "W") :west)))


(defun distance (row1 col1 row2 col2)
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (- *map-rows* drow)))
         (mincol (min dcol (- *map-cols* dcol))))
    (sqrt (+ (* minrow minrow) (* mincol mincol)))))


(defun do-turn (turn)
  (setf *orders* nil)
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
                                 ;(logmsg i ":" bot ": go~%")
                                 (loop-finish))
                               ;(logmsg i ":" bot ": " line "~%")
                               (when (starts-with line "o ")
                                 (queue-ant-order i line))))))))
  (move-ants))


(defun move-ants ()
  (clear-dead-ants)
  (check-positions)
  (check-water)
  (check-collisions)
  (loop for order in *orders*
        for bot-id = (getf order :bot-id)
        for src-row = (getf order :src-row)
        for src-col = (getf order :src-col)
        for dst-row = (getf order :dst-row)
        for dst-col = (getf order :dst-col)
        do (setf (aref *map-array* src-row src-col) 0)
           (setf (aref *map-array* dst-row dst-col) (+ bot-id 100)))
  (battle-resolution)
  (spawn-ants))


(defun new-location (row col direction)
  (if (not (member direction '(:north :east :south :west)))
      (progn (logmsg "[new-location] Illegal direction: " direction "~%")
             (list row col))
      (let ((dst-row (cond ((equal direction :north)
                            (if (<= row 0)
                                (- *map-rows* 1)
                                (- row 1)))
                           ((equal direction :south)
                            (if (>= (+ row 1) *map-rows*)
                                0
                                (+ row 1)))
                           (t row)))
            (dst-col (cond ((equal direction :east)
                            (if (>= (+ col 1) *map-cols*)
                                0
                                (+ col 1)))
                           ((equal direction :west)
                            (if (<= col 0)
                                (- *map-cols* 1)
                                (- col 1)))
                           (t col))))
        (list dst-row dst-col))))


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
                    (exit 1))
                   ((and (starts-with line "m ") (null *map-rows*))
                    (logmsg "~&Map missing \"rows n\" line. Aborting...~%")
                    (exit 1))
                   ((and (starts-with line "m ") (null *map-players*))
                    (logmsg "~&Map missing \"players n\" line. Aborting...~%")
                    (exit 1))
                   ((and (starts-with line "m ")
                         (< (length *bots*) *map-players*))
                    (logmsg "~&Map needs " *map-players* " players but only "
                            (length *bots*) " were entered on the "
                            "command-line. Aborting...~%")
                    (exit 1))
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
                       (exit 1)))))


(defun parse-map-line (map-array string row)
  (when (/= (- (length string) 2) *map-cols*)
    (logmsg "~&Actual map columns (" (- (length string) 2) ") for this line "
            "not equal to specified number of~%columns (" *map-cols* ") for "
            "this map. Aborting...~%")
    (exit 1))
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


(defun queue-ant-order (bot-id string)
  (let* ((split (split-sequence #\space (string-upcase string)))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (dir (dir2key (elt split 3)))
         (nl (new-location row col dir)))
    (push (list :bot-id bot-id :src-row row :src-col col :dst-row (elt nl 0)
                :dst-col (elt nl 1) :direction dir)
          *orders*)))


(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


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


(defun send-game-state (bot-id input turn)
  (format input "turn ~D~%" turn)
  (loop for row from 0 below *map-rows*
        do (loop for col from 0 below *map-cols*
                 for sq = (aref *map-array* row col)
                 do (cond ((= sq 1) (format input "w ~D ~D~%" row col))
                          ((= sq 2) (format input "f ~D ~D~%" row col))
                          ((>= sq 200)
                           (format input "d ~D ~D ~D~%" row col
                                   (cond ((= bot-id (- sq 200)) 0)
                                         ((< bot-id (- sq 200)) (- sq 200))
                                         ((> bot-id (- sq 200)) (- sq 199)))))
                          ((>= sq 100)
                           (format input "a ~D ~D ~D~%" row col
                                   (cond ((= bot-id (- sq 100)) 0)
                                         ((< bot-id (- sq 100)) (- sq 100))
                                         ((> bot-id (- sq 100)) (- sq 99))))))))
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


;; TODO very innefficient: fix
(defun spawn-ants ()
  (let ((ants (loop for row from 0 below *map-rows*
                    append (loop for col from 0 below *map-cols*
                                 for value = (aref *map-array* row col)
                                 when (and (>= value 100) (<= value 199))
                                   collect (list value row col))))
        (foods (loop for row from 0 below *map-rows*
                     append (loop for col from 0 below *map-cols*
                                  when (= 2 (aref *map-array* row col))
                                    collect (list row col)))))
    (loop for food in foods
          for frow = (elt food 0)
          for fcol = (elt food 1)
          for nearby-ant-ids = nil
          do (loop for ant in ants
                   for aid = (elt ant 0)
                   for arow = (elt ant 1)
                   for acol = (elt ant 2)
                   do (when (<= (distance frow fcol arow acol) *spawn-radius*)
                        (pushnew aid nearby-ant-ids))
                   finally (cond ((= 1 (length nearby-ant-ids))
                                  (setf (aref *map-array* frow fcol)
                                        (first nearby-ant-ids)))
                                 ((> (length nearby-ant-ids) 1)
                                  ;(logmsg "Multiple contestants for food at "
                                  ;        frow ":" fcol ". Removing...~%")
                                  (setf (aref *map-array* frow fcol) 0)))))))


;; Very simple random food spawning.  Collects all land tile's coordinates
;; and randomly picks a number of tiles equal to the number of players.
(defun spawn-food ()
  (let ((food (loop for row from 0 below *map-rows*
                    append (loop for col from 0 below *map-cols*
                                 when (= 0 (aref *map-array* row col))
                                   collect (list row col))
                      into result
                    finally (return (loop repeat *map-players*
                                          collect (random-elt result))))))
    (loop for rc in food
          for row = (elt rc 0)
          for col = (elt rc 1)
          do (setf (aref *map-array* row col) 2))))


(defun no-turn-time-left-p (turn-start-time)
  (not (turn-time-left-p turn-start-time)))


(defun turn-time-left-p (turn-start-time)
  (<= (- (wall-time) turn-start-time)
      (/ *turn-time* 1000)))


(defun wait-for-output (output turn-start-time)
  (loop until (or (listen output)
                  (no-turn-time-left-p turn-start-time))))


(defun water? (row col direction)
  (let ((nl (new-location row col direction)))
    (= 1 (aref *map-array* (elt nl 0) (elt nl 1)))))


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
  (handler-bind (#+sbcl (sb-sys:interactive-interrupt #'user-interrupt))
                 ;(error #'error-handler))
    (loop for bot in *bots* collect (run-program bot) into procs
          finally (setf *procs* procs))
    (loop for turn from 0 to *turns*
          do (logmsg "--- turn: " turn " ---~%")
             (when (> turn 0) (spawn-food))
             (when *verbose* (print-game-map *map-array* *debug*))
             (when (= turn 0) (send-initial-game-state))
             (when (> turn 0) (do-turn turn))))
  (logmsg "[  end] " (current-date-time-string) "~%"))
