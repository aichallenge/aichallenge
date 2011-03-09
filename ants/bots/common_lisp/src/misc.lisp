;;;; misc.lisp

(in-package :ants-bot)


;;; Functions

(defun end-of-turn (output)
  (format output "~&go~%"))


(defun move-ant (bot ant-x ant-y direction)
  (if (not (member direction '(:north :east :south :west)))
      (errmsg "[move-ant] Illegal direction: " direction)
      (format (output bot) "~&o ~D ~D ~A~%" ant-y ant-x
              (case direction
                (:north "N")
                (:east  "E")
                (:south "S")
                (:west  "W")))))


(defun waterp (direction bot x y)
  (case direction
    (:north (= 1 (aref (game-map bot)
                       (if (= y 0)
                           (- (rows bot) 1)
                           (- y 1))
                       x)))
    (:east (= 1 (aref (game-map bot)
                      y
                      (if (>= (+ x 1) (cols bot))
                          0
                          (+ x 1)))))
    (:south (= 1 (aref (game-map bot)
                       (if (>= (+ y 1) (rows bot))
                           0
                           (+ y 1))
                       x)))
    (:west (= 1 (aref (game-map bot)
                      y
                      (if (= x 0)
                          (- (cols bot) 1)
                          (- x 1)))))
    (otherwise (error "Unknown direction ~S" direction))))
