;;;; proxy-bot.asd

(in-package :cl-user)

(asdf:defsystem :proxy-bot
  :components ((:module src
                :serial t
                :components ((:file "proxy-bot"))))
  :depends-on (:sb-bsd-sockets))
