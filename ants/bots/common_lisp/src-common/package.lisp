;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-common
  (:use :cl)
  (:export ;; specials
           :*state* :*verbose*
           ;; classes and accessors / readers
           :state :attack-radius2 :cols :enemy-ants :food :game-map :input
           :load-time :log-stream :my-ants :output :rows :spawn-radius2 :turn
           :turn-start-time :turn-time :turns :view-radius2
           ;; handlers
           :address-in-use :connection-lost :connection-refused :error-handler
           :socket-error-handler :user-interrupt
           ;; functions
           :current-date-time-string :distance :errmsg :host2str :logmsg :mkstr
           :new-location :par-value :print-game-map :quit :starts-with
           :wall-time :water?))
