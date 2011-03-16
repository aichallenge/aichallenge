;;;; misc.lisp

(in-package :ants-bot)


;;; Functions

(defun distance (row1 col1 row2 col2)
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (- (rows *state*) drow)))
         (mincol (min dcol (- (cols *state*) dcol))))
    (sqrt (+ (* minrow minrow) (* mincol mincol)))))


(defun end-of-turn ()
  (format (output *state*) "~&go~%")
  (force-output (output *state*)))


(defun errmsg (&rest args)
  (format *error-output* (with-output-to-string (s)
                           (dolist (a args)
                             (princ a s)))))


(defun logmsg (&rest args)
  (when *verbose*
    (format (log-stream *state*) (apply #'mkstr args))
    (force-output (log-stream *state*))))


(defun move-ant (row col direction)
  (if (not (member direction '(:north :east :south :west)))
      (errmsg "[move-ant] Illegal direction: " direction)
      (format (output *state*) "~&o ~D ~D ~A~%" row col
              (case direction
                (:north "N")
                (:east  "E")
                (:south "S")
                (:west  "W")))))


(defun new-location (row col direction)
  (if (not (member direction '(:north :east :south :west)))
      (progn (errmsg "[new-location] Illegal direction: " direction)
             (list row col))
      (let ((dst-row (cond ((equal direction :north)
                            (if (<= row 0)
                                (- (rows *state*) 1)
                                (- row 1)))
                           ((equal direction :south)
                            (if (>= (+ row 1) (rows *state*))
                                0
                                (+ row 1)))
                           (t row)))
            (dst-col (cond ((equal direction :east)
                            (if (>= (+ col 1) (cols *state*))
                                0
                                (+ col 1)))
                           ((equal direction :west)
                            (if (<= col 0)
                                (- (cols *state*) 1)
                                (- col 1)))
                           (t col))))
        (list dst-row dst-col))))


(defun turn-time-remaining ()
  (- (+ (turn-start-time *state*) (turn-time *state*))
     (wall-time)))


(defun turn-time-used ()
  (- (wall-time) (turn-start-time *state*)))


(defun water? (row col direction)
  (let ((nl (new-location row col direction)))
    (= 1 (aref (game-map *state*) (elt nl 0) (elt nl 1)))))
