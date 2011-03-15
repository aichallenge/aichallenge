;;;; proxy-bot.asd

(in-package :cl-user)

(asdf:defsystem :proxy-bot
  :components ((:module src-proxy-bot
                :serial t
                :components ((:file "package")
                             (:file "proxy-bot"))))
  :depends-on (:ants-common :com.dvlsoft.clon :usocket))
