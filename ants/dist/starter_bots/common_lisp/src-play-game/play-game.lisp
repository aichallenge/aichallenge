;;;; play-game.lisp
;;;;
;;;; This is really a hack job currently.
;;;;
;;;; We should keep seperate lists of ants for each player and also for food
;;;; so resolutions can be checked quicker than going over the map every time.

(in-package :play-game)


;;; Functions

(defun ants-within-attack-range ()
  (loop with all = (loop for row from 0 below (rows *state*)
                     append (loop for col from 0 below (cols *state*)
                                  for value = (aref (game-map *state*) row col)
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
                               (<= dist (sqrt (attack-radius2 *state*))))
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
        for aid = (when battle (- (aref (game-map *state*) arow acol) 99))
        for brow = (getf battle :b-row)
        for bcol = (getf battle :b-col)
        for bid = (when battle (- (aref (game-map *state*) brow bcol) 99))
        do (logmsg "Ants " aid ":" arow ":" acol " and " bid ":" brow ":" bcol
                   " fought...~%")
           (setf (aref (game-map *state*) arow acol) (+ aid 199)
                 (aref (game-map *state*) brow bcol) (+ bid 199))))


(defun battle-resolution ()
  (battle-resolution2))


;; TODO fix inefficient algorithm
(defun check-collisions ()
  (loop for order-a in (copy-seq (orders *state*))
        for bot-a-id = (getf order-a :bot-id)
        for srow-a = (getf order-a :src-row)
        for scol-a = (getf order-a :src-col)
        for row-a = (getf order-a :dst-row)
        for col-a = (getf order-a :dst-col)
        do (loop for order-b in (remove order-a (copy-seq (orders *state*)))
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
                      (setf (aref (game-map *state*) srow-a scol-a) 0
                            (aref (game-map *state*) srow-b scol-b) 0
                            (orders *state*) (remove order-b (remove order-a (orders *state*))))))))


(defun check-positions ()
  (loop for order in (copy-seq (orders *state*))
        for bot-id = (getf order :bot-id)
        for row = (getf order :src-row)
        for col = (getf order :src-col)
        do (when (/= (+ bot-id 100) (aref (game-map *state*) row col))
             (logmsg "Bot " bot-id " issued an order for a position it "
                     "doesn't occupy. Ignoring...~%")
             (setf (orders *state*) (remove order (orders *state*))))))


(defun check-water ()
  (loop for order in (copy-seq (orders *state*))
        for bot-id = (getf order :bot-id)
        for row = (getf order :src-row)
        for col = (getf order :src-col)
        for dir = (getf order :direction)
        do (when (water? row col dir)
             (logmsg "Bot " bot-id " ordered an ant into water. Ignoring...~%")
             (setf (orders *state*) (remove order (orders *state*))))))


(defun clear-dead-ants ()
  (loop for row from 0 below (rows *state*)
        do (loop for col from 0 below (cols *state*)
                 do (when (>= (aref (game-map *state*) row col) 200)
                      (setf (aref (game-map *state*) row col) 0)))))


(defun dir2key (direction)
  (cond ((equal direction "N") :north)
        ((equal direction "E") :east)
        ((equal direction "S") :south)
        ((equal direction "W") :west)))


(defun do-turn (turn)
  (setf (orders *state*) nil)
  (loop for proc in (procs *state*)
        for i from 0
        for bot = (elt (bots *state*) i)
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
  (loop for order in (orders *state*)
        for bot-id = (getf order :bot-id)
        for src-row = (getf order :src-row)
        for src-col = (getf order :src-col)
        for dst-row = (getf order :dst-row)
        for dst-col = (getf order :dst-col)
        do (setf (aref (game-map *state*) src-row src-col) 0)
           (setf (aref (game-map *state*) dst-row dst-col) (+ bot-id 100)))
  (battle-resolution)
  (spawn-ants))


(defun parse-map (file-name)
  (with-open-file (f file-name)
    (loop with rows = 0
          for line = (read-line f nil)
          while line
          do (cond ((starts-with line "cols ")
                    (setf (slot-value *state* 'cols) (par-value line)))
                   ((starts-with line "rows ")
                    (setf (slot-value *state* 'rows) (par-value line)))
                   ((starts-with line "players ")
                    (setf (slot-value *state* 'players) (par-value line)))
                   ((and (starts-with line "m ") (null (cols *state*)))
                    (logmsg "~&Map missing \"cols n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (rows *state*)))
                    (logmsg "~&Map missing \"rows n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (players *state*)))
                    (logmsg "~&Map missing \"players n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ")
                         (< (length (bots *state*)) (players *state*)))
                    (logmsg "~&Map needs " (players *state*) " players but "
                            "only " (length (bots *state*)) " were entered on "
                            "the command-line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (game-map *state*)))
                    (setf (slot-value *state* 'game-map)
                          (make-array (list (rows *state*) (cols *state*))
                                     :element-type 'fixnum :initial-element 0))
                    (parse-map-line (game-map *state*) line rows)
                    (incf rows))
                   ((starts-with line "m ")
                    (parse-map-line (game-map *state*) line rows)
                    (incf rows)))
             finally (when (/= rows (rows *state*))
                       (logmsg "~&Actual map rows (" rows ") not equal to "
                               "specified number of rows (" (rows *state*)
                               "). Aborting...~%")
                       (quit 1)))))


(defun parse-map-line (map-array string row)
  (when (/= (- (length string) 2) (cols *state*))
    (logmsg "~&Actual map columns (" (- (length string) 2) ") for this line "
            "not equal to specified number of~%columns (" (cols *state*) ") "
            "for this map. Aborting...~%")
    (quit 1))
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
         (quit))
        ;; use :SAVE-RUNTIME-OPTIONS to SAVE-LISP-AND-DIE if you want --version
        ((getopt :long-name "release")
         (format t "~&play-game (Ant Wars) version ~A~%" +version+)
         (quit)))
  (do-cmdline-options (option name value source)
    (setf source source)  ; to silence compilation warnings  TODO remove!
    (cond ((or (equal name "m") (equal name "map_file"))
           (setf (slot-value *state* 'map-file) value))
          ((or (equal name "r") (equal name "rounds"))
           (setf (slot-value *state* 'turns) (parse-integer value)))
          ((or (equal name "t") (equal name "turns"))
           (setf (slot-value *state* 'turns) (parse-integer value)))
          ((or (equal name "v") (equal name "verbose"))
           (setf *verbose* t))
          ((equal name "loadtime")
           (setf (slot-value *state* 'load-time) (parse-integer value)))
          ((equal name "turntime")
           (setf (slot-value *state* 'turn-time) (parse-integer value)))))
  (unless (map-file *state*)
    (help)
    (quit))
  (setf (slot-value *state* 'bots) (loop for bot in (remainder) collect bot)))


(defun queue-ant-order (bot-id string)
  (let* ((split (split-sequence #\space (string-upcase string)))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (dir (dir2key (elt split 3)))
         (nl (new-location row col dir)))
    (push (list :bot-id bot-id :src-row row :src-col col :dst-row (elt nl 0)
                :dst-col (elt nl 1) :direction dir)
          (orders *state*))))


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
  (loop for row from 0 below (rows *state*)
        do (loop for col from 0 below (cols *state*)
                 for sq = (aref (game-map *state*) row col)
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
  (loop for proc in (procs *state*)
        for i from 0
        for bot = (elt (bots *state*) i)
        for pin = (sb-ext:process-input proc)
        for pout = (sb-ext:process-output proc)
        do (if (equal :running (sb-ext:process-status proc))
               (progn
                 (format pin "turn 0~%loadtime ~D~%turntime ~D~%rows ~D~%cols ~D~%turns ~D~%viewradius2 ~D~%attackradius2 ~D~%spawnradius2 ~D~%ready~%"
                         (load-time *state*) (turn-time *state*) (rows *state*)
                         (cols *state*) (turns *state*)
                         (view-radius2 *state*)
                         (attack-radius2 *state*)
                         (spawn-radius2 *state*))
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
  (let ((ants (loop for row from 0 below (rows *state*)
                    append (loop for col from 0 below (cols *state*)
                                 for value = (aref (game-map *state*) row col)
                                 when (and (>= value 100) (<= value 199))
                                   collect (list value row col))))
        (foods (loop for row from 0 below (rows *state*)
                     append (loop for col from 0 below (cols *state*)
                                  when (= 2 (aref (game-map *state*) row col))
                                    collect (list row col)))))
    (loop for food in foods
          for frow = (elt food 0)
          for fcol = (elt food 1)
          for nearby-ant-ids = nil
          do (loop for ant in ants
                   for aid = (elt ant 0)
                   for arow = (elt ant 1)
                   for acol = (elt ant 2)
                   do (when (<= (distance frow fcol arow acol)
                                (sqrt (spawn-radius2 *state*)))
                        (pushnew aid nearby-ant-ids))
                   finally (cond ((= 1 (length nearby-ant-ids))
                                  (setf (aref (game-map *state*) frow fcol)
                                        (first nearby-ant-ids)))
                                 ((> (length nearby-ant-ids) 1)
                                  ;(logmsg "Multiple contestants for food at "
                                  ;        frow ":" fcol ". Removing...~%")
                                  (setf (aref (game-map *state*) frow fcol)
                                        0)))))))


;; Very simple random food spawning.  Collects all land tile's coordinates
;; and randomly picks a number of tiles equal to the number of players.
(defun spawn-food ()
  (let ((food (loop for row from 0 below (rows *state*)
                    append (loop for col from 0 below (cols *state*)
                                 when (= 0 (aref (game-map *state*) row col))
                                   collect (list row col))
                      into result
                    finally (return (loop repeat (players *state*)
                                          collect (random-elt result))))))
    (loop for rc in food
          for row = (elt rc 0)
          for col = (elt rc 1)
          do (setf (aref (game-map *state*) row col) 2))))


(defun no-turn-time-left-p (turn-start-time)
  (not (turn-time-left-p turn-start-time)))


(defun turn-time-left-p (turn-start-time)
  (<= (- (wall-time) turn-start-time)
      (/ (turn-time *state*) 1000)))


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
  (let ((*state* (make-instance 'play-game-state :error-stream *debug-io*))
        (*verbose* nil))
    (process-cmdline-options)
    (parse-map (map-file *state*))
    (let ((cdts (current-date-time-string)))
      (logmsg "~&=== New Match: " cdts " ===~%")
      (logmsg "[start] " cdts "~%"))
    (handler-bind (#+sbcl (sb-sys:interactive-interrupt #'user-interrupt))
                   ;(error #'error-handler))
      (loop for bot in (bots *state*) collect (run-program bot) into procs
            finally (setf (slot-value *state* 'procs) procs))
      (loop for turn from 0 to (turns *state*)
            do (setf (slot-value *state* 'turn) turn)
               (logmsg "--- turn: " turn " ---~%")
               (when (> turn 0) (spawn-food))
               (when *verbose*
                 (print-game-map (game-map *state*) (log-stream *state*)))
               (when (= turn 0) (send-initial-game-state))
               (when (> turn 0) (do-turn turn))))
    (logmsg "[  end] " (current-date-time-string) "~%")))
