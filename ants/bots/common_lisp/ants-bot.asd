;;;; ants-bot.asd

(in-package :cl-user)

(asdf:defsystem :ants-bot
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "specials")
                             (:file "misc")
                             (:file "game-state")
                             (:file "bot-think")
                             (:file "ants-bot"))))
  :depends-on (:ants-common :usocket))
