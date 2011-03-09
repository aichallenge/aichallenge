;;;; misc.lisp

(in-package :ants-bot)


;;; Functions

;; TODO hasn't been tested very well yet
(defun distance (x1 y1 x2 y2)
  (let* ((dx (abs (- x1 x2)))
         (dy (abs (- y1 y2)))
         (minx (min dx (- (cols *state*) dx)))
         (miny (min dy (- (rows *state*) dy))))
    (sqrt (+ (* minx minx) (* miny miny)))))


(defun end-of-turn ()
  (format (output *state*) "~&go~%"))


(defun move-ant (ant-x ant-y direction)
  (if (not (member direction '(:north :east :south :west)))
      (errmsg "[move-ant] Illegal direction: " direction)
      (format (output *state*) "~&o ~D ~D ~A~%" ant-y ant-x
              (case direction
                (:north "N")
                (:east  "E")
                (:south "S")
                (:west  "W")))))


;; TODO hasn't been tested very well yet
(defun new-location (src-x src-y direction)
  (if (not (member direction '(:north :east :south :west)))
      (progn (errmsg "[new-location] Illegal direction: " direction)
             (list src-x src-y))
      (let ((dst-x (cond ((equal direction :east)
                          (if (>= (+ src-x 1) (cols *state*))
                              0
                              (+ src-x 1)))
                         ((equal direction :west)
                          (if (<= src-x 0)
                              (- (cols *state*) 1)
                              (- src-x 1)))
                         (t src-x)))
            (dst-y (cond ((equal direction :north)
                          (if (<= src-y 0)
                              (- (rows *state*) 1)
                              (- src-y 1)))
                         ((equal direction :south)
                          (if (>= (+ src-y 1) (rows *state*))
                              0
                              (+ src-y 1)))
                         (t src-y))))
        (list dst-x dst-y))))


(defun print-game-map (game-map &optional (stream *debug-io*))
  (loop with dim = (array-dimensions game-map)
        for y from 0 below (first dim)
        do (loop for x from 0 below (second dim)
                 for val = (aref game-map y x)
                 do (cond ((= val 0) (princ #\. stream))
                          ((= val 1) (princ #\% stream))
                          ((= val 2) (princ #\* stream))
                          ((>= val 200) (princ (code-char (- val 135)) stream))
                          ((>= val 100) (princ (code-char (- val 3)) stream))
                          (t (princ #\? stream))))
           (terpri stream)))


;; TODO hasn't been tested very well yet
(defun turn-time-remaining ()
  (- (wall-time)
     (+ (turn-start-time *state*) (turn-time *state*))))


;; TODO hasn't been tested very well yet
(defun turn-time-used ()
  (- (wall-time) (turn-start-time *state*)))


;; TODO hasn't been tested very well yet
(defun water? (x y direction)
  (let ((nl (new-location x y direction)))
    (= 1 (aref (game-map *state*) (elt nl 1) (elt nl 0)))))
