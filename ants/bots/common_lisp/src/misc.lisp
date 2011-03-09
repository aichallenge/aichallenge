;;;; misc.lisp

(in-package :ants-bot)


;;; Functions

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


(defun water? (x y direction)
  (case direction
    (:north (= 1 (aref (game-map *state*)
                       (if (= y 0)
                           (- (rows *state*) 1)
                           (- y 1))
                       x)))
    (:east (= 1 (aref (game-map *state*)
                      y
                      (if (>= (+ x 1) (cols *state*))
                          0
                          (+ x 1)))))
    (:south (= 1 (aref (game-map *state*)
                       (if (>= (+ y 1) (rows *state*))
                           0
                           (+ y 1))
                       x)))
    (:west (= 1 (aref (game-map *state*)
                      y
                      (if (= x 0)
                          (- (cols *state*) 1)
                          (- x 1)))))
    (otherwise (error "Unknown direction ~S" direction))))
