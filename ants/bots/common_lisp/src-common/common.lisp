;;;; common.lisp

(in-package :ants-common)


;;; Functions

(defun current-date-time-string ()
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))


(defun distance (row1 col1 row2 col2)
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (- (rows *state*) drow)))
         (mincol (min dcol (- (cols *state*) dcol))))
    (sqrt (+ (* minrow minrow) (* mincol mincol)))))


(defun errmsg (&rest args)
  (format (error-stream *state*) (apply #'mkstr args))
  (force-output *error-output*))


(defun host2str (host)
  (cond ((and (vectorp host) (= 4 (length host)))
         (format nil "~D.~D.~D.~D" (elt host 0) (elt host 1) (elt host 2)
                 (elt host 3)))
        (t host)))


(defun logmsg (&rest args)
  (when *verbose*
    (format (log-stream *state*) (apply #'mkstr args))
    (force-output (log-stream *state*))))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))


(defun new-location (row col direction)
  (if (not (member direction '(:north :east :south :west)))
      (progn (logmsg "[new-location] Illegal direction: " direction "~%")
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


(defun par-value (string)
  (parse-integer (subseq string (position #\space string) (length string))))


(defun print-game-map (game-map &optional (stream *debug-io*))
  (loop with dim = (array-dimensions game-map)
        for col from 0 below (second dim)
        initially (princ #\space stream)
                  (princ #\space stream)
        do (princ (mod col 10) stream)
        finally (terpri stream))
  (loop with dim = (array-dimensions game-map)
        for row from 0 below (first dim)
        do (princ (mod row 10) stream)
           (princ #\space stream)
           (loop for col from 0 below (second dim)
                 for val = (aref game-map row col)
                 do (cond ((= val 0) (princ #\. stream))
                          ((= val 1) (princ #\% stream))
                          ((= val 2) (princ #\* stream))
                          ((>= val 200) (princ (code-char (- val 135)) stream))
                          ((>= val 100) (princ (code-char (- val 3)) stream))
                          (t (princ #\? stream))))
           (terpri stream)))


;; grabbed from Clon
(defun quit (&optional (status 0))
  "Quit the current application with STATUS."
  #+sbcl  (sb-ext:quit :unix-status status)
  #+cmu   (unix:unix-exit status)
  #+ccl   (ccl:quit status)
  #+ecl   (ext:quit status)
  #+clisp (ext:exit status)
  #+abcl  (extensions:exit :status status)
  #-(and sbcl cmu ccl ecl clisp abcl) (cl-user::quit))


(defun starts-with (sequence subsequence)
  (let ((sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen (length sequence)))
      (equal (subseq sequence 0 sublen) subsequence))))


(let ((time-units (/ 1.0 internal-time-units-per-second)))
  (defun wall-time (&key (offset 0))
    (+ (* (get-internal-real-time) time-units)
       offset)))


(defun water? (row col direction)
  (let ((nl (new-location row col direction)))
    (= 1 (aref (game-map *state*) (elt nl 0) (elt nl 1)))))
